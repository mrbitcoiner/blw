package com.lightning.walletapp.lnutils.olympus

import spray.json._
import com.lightning.walletapp.R.string._
import com.github.kevinsawicki.http.HttpRequest._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import java.math.BigInteger
import scodec.bits.ByteVector
import java.net.ProtocolException
import fr.acinq.bitcoin.Transaction
import com.lightning.walletapp.Utils.app
import com.lightning.walletapp.ln.Tools.none
import com.lightning.walletapp.ln.LNParams.db
import com.lightning.walletapp.ln.PaymentRequest
import com.lightning.walletapp.helper.RichCursor
import scala.collection.JavaConverters.mapAsJavaMapConverter
import com.lightning.walletapp.ln.RoutingInfoTag.PaymentRouteVec
import com.lightning.walletapp.lnutils.{OlympusLogTable, OlympusTable, RevokedInfoTable}
import com.lightning.walletapp.ln.wire.{NodeAnnouncement, OutRequest}
import rx.lang.scala.{Observable => Obs}


object OlympusWrap {
  type RequestAndMemo = (PaymentRequest, BlindMemo)
  type AnnounceChansNum = (NodeAnnouncement, Int)
  type ClearToken = (String, String, String)
  type TokensInfo = (String, String, Int)
  type HttpParam = (String, String)

  // Shortcuts for Olympus RPC return data types
  type AnnounceChansNumVec = Vector[AnnounceChansNum]
  type BigIntegerVec = Vector[BigInteger]
  type CloudActVec = Vector[CloudAct]
  type StringVec = Vector[String]
  type CloudVec = Vector[Cloud]

  // Tx request/response
  type ByteVecSeq = Seq[ByteVector]
  type TxSeq = Seq[Transaction]

  // BTC and fiat rates
  type Fiat2Btc = Map[String, Double]
  type BlockNum2Fee = Map[String, Double]
  type Result = (BlockNum2Fee, Fiat2Btc)
  val CMDStart = "CMDStart"
  val BODY = "body"

  def toCloud(rc: RichCursor) = {
    val auth = rc int OlympusTable.auth
    val id = rc string OlympusTable.identifier
    val removable = rc int OlympusTable.removable
    val stored = to[CloudData](rc string OlympusTable.data)
    val connector = new Connector(rc string OlympusTable.url)
    new Cloud(id, connector, auth, removable) { data = stored }
  }
}

class OlympusWrap extends OlympusProvider {
  // All available clouds for RPC queries and backups
  // backup upload requests are also sent to all the clounds
  // and final filtering is done inside of each available cloud
  var clouds = RichCursor(db select OlympusTable.selectAllSql) vec toCloud
  def tellClouds(candidateData: Any) = for (cloud <- clouds) cloud doProcess candidateData
  def pendingWatchTxIds = clouds.flatMap(_.data.acts).collect { case ca: CerberusAct => ca.txids }.flatten

  // SQL interface

  def remove(identifier: String) = db.change(OlympusTable.killSql, identifier)
  def updData(data: String, identifier: String) = db.change(OlympusTable.updDataSql, data, identifier)
  def updMeta(cd: Cloud, order: Int) = db.change(OlympusTable.updMetaSql, cd.connector.url, cd.auth, order, cd.identifier)

  def addServer(cloud: Cloud, order: Int) =
    db.change(OlympusTable.newSql, cloud.identifier, cloud.connector.url,
      cloud.data.toJson.toString, cloud.auth, order, cloud.removable)

  // Olympus RPC interface

  def failOver[T](run: Cloud => Obs[T], onRunOut: Obs[T], cs: CloudVec): Obs[T] = {
    def tryAgainWithNextCloud(failure: Throwable) = failOver(run, onRunOut, cs.tail)
    if (cs.isEmpty) onRunOut else run(cs.head) onErrorResumeNext tryAgainWithNextCloud
  }

  def getBackup(key: String) = {
    def empty(failure: Throwable) = Vector.empty[String]
    // Special case: we need to query all the available clouds at once
    Obs.from(clouds).flatMap(_.connector getBackup key onErrorReturn empty)
  }

  def findNodes(query: String) = failOver(_.connector findNodes query, Obs.empty, clouds)
  def findRoutes(out: OutRequest) = failOver(_.connector findRoutes out, Obs just Vector.empty, clouds)
  def getRates = failOver(_.connector.getRates, Obs error new ProtocolException("Could not obtain feerates and fiat prices"), clouds)
  def getChildTxs(ids: ByteVecSeq) = failOver(_.connector getChildTxs ids, Obs error new ProtocolException("Try again later"), clouds)
}

trait OlympusProvider {
  def findRoutes(out: OutRequest): Obs[PaymentRouteVec]
  def findNodes(query: String): Obs[AnnounceChansNumVec]
  def getChildTxs(txIds: ByteVecSeq): Obs[TxSeq]
  def getBackup(key: String): Obs[StringVec]
  def getRates: Obs[Result]
}

class Connector(val url: String) extends OlympusProvider {
  def ask[T: JsonFormat](commandPath: String, parameters: HttpParam*): Obs[T] =
    queue.map(_ => http(commandPath).form(parameters.toMap.asJava).body.parseJson) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: response +: _) => response.convertTo[T]
      case _ => throw new ProtocolException
    }

  def getRates = ask[Result]("rates/get")
  def getBackup(key: String) = ask[StringVec]("data/get", "key" -> key)
  def findNodes(query: String) = ask[AnnounceChansNumVec]("router/nodes", "query" -> query)
  def getChildTxs(txIds: ByteVecSeq) = ask[TxSeq]("txs/get", "txids" -> txIds.toJson.toString.s2hex)
  def findRoutes(out: OutRequest) = ask[PaymentRouteVec]("router/routesplus", "params" -> out.toJson.toString.s2hex)
  def http(requestPath: String) = post(s"$url/$requestPath", true).trustAllCerts.trustAllHosts.connectTimeout(15000)
}

// CLOUD UPLOAD ACTS

trait CloudAct {
  def onDone: Unit
  val plus: Seq[HttpParam]
  val data: ByteVector
  val path: String
}

case class CloudData(info: Option[RequestAndMemo], tokens: Vector[ClearToken], acts: CloudActVec)
case class LegacyAct(data: ByteVector, plus: Seq[HttpParam], path: String) extends CloudAct { def onDone = none }
case class CerberusAct(data: ByteVector, plus: Seq[HttpParam], path: String, txids: StringVec) extends CloudAct {
  // This is an act for uploading a pack of RevocationInfo objects, affected records should be marked once uploaded

  def onDone = db txWrap {
    val text = app.getString(olympus_log_watch_payments).format(txids.size)
    for (oldTxid <- txids) db.change(RevokedInfoTable.setUploadedSql, oldTxid)
    db.change(OlympusLogTable.newSql, params = 1, text, System.currentTimeMillis)
  }
}

case class ChannelUploadAct(data: ByteVector, plus: Seq[HttpParam], path: String, alias: String) extends CloudAct {
  // This is an act for uploading a channel encrypted backup immediately once a transaction for a new channel gets broadcasted
  def onDone = db.change(OlympusLogTable.newSql, 1, app.getString(olympus_log_channel_backup).format(alias), System.currentTimeMillis)
}