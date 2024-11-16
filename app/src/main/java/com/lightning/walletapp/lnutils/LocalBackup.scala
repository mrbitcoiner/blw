package com.lightning.walletapp.lnutils

import spray.json._
import android.os.Environment._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.JsonHttpUtils.to
import com.lightning.walletapp.ln.wire.LocalBackups
import android.support.v4.content.ContextCompat
import android.support.v7.app.AppCompatActivity
import android.support.v4.app.ActivityCompat
import com.lightning.walletapp.helper.AES
import android.content.pm.PackageManager
import com.google.common.io.Files
import org.bitcoinj.wallet.Wallet
import fr.acinq.bitcoin.Block
import scodec.bits.ByteVector
import scala.util.Try
import java.io.File


object LocalBackup {
  final val BACKUP_DIR = "BLW"
  final val BACKUP_FILE_NAME = "encrypted.channels.bkp"
  final val LOCAL_BACKUP_REQUEST_NUMBER = 105

  def isExternalStorageWritable: Boolean = {
    val isMounted = MEDIA_MOUNTED.equals(getExternalStorageState)
    val isWritable = getExternalStorageDirectory.canWrite
    isMounted && isWritable
  }

  def askPermission(activity: AppCompatActivity) = ActivityCompat.requestPermissions(activity, Array(android.Manifest.permission.WRITE_EXTERNAL_STORAGE), LOCAL_BACKUP_REQUEST_NUMBER)
  def isAllowed(activity: AppCompatActivity) = ContextCompat.checkSelfPermission(activity, android.Manifest.permission.WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED

  def getBackupDirectory(chainHash: ByteVector) = chainHash match {
    case Block.LivenetGenesisBlock.hash => "mainnet"
    case Block.TestnetGenesisBlock.hash => "testnet"
    case Block.RegtestGenesisBlock.hash => "regtest"
    case _ => "unknown"
  }

  def getBackupFileUnsafe(channelBackupDirName: String, mnemonicSuffix: String): File = {
    val chainDir = new File(new File(getExternalStorageDirectory, BACKUP_DIR), channelBackupDirName)
    val backup = new File(chainDir, s"$BACKUP_FILE_NAME-$mnemonicSuffix")
    if (!backup.isFile) chainDir.mkdirs
    backup
  }

  def encryptAndWrite(file: File, channels: Vector[Channel], wallet: Wallet, secret: ByteVector) = {
    // Persist normal and hosted channels except those with RefundingData for which we don't have a remote point
    val datas = channels.map(_.data).filter { case refund: RefundingData => refund.remoteLatestPoint.isDefined case _ => true }

    val backup = (LocalBackups(Vector.empty, Vector.empty, wallet.getEarliestKeyCreationTime, v = 1) /: datas) {
      case (LocalBackups(normal, hosted, secs, v), hasNorm: HasNormalCommits) => LocalBackups(normal :+ hasNorm, hosted, secs, v)
      case (LocalBackups(normal, hosted, secs, v), hostedCommits: HostedCommits) => LocalBackups(normal, hosted :+ hostedCommits, secs, v)
      case (updatedLocalBackups, _) => updatedLocalBackups
    }

    val encrypted = AES.encReadable(backup.toJson.toString, secret.toArray)
    Files.write(encrypted.toByteArray, file)
  }

  def readAndDecrypt(file: File, secret: ByteVector) = for {
    encryptedLocalBackupByteArray <- Try(Files toByteArray file)
    plaintextJson <- AES.decBytes(encryptedLocalBackupByteArray, secret.toArray)
    decryptedBackup = to[LocalBackups](Tools bin2readable plaintextJson)
  } yield decryptedBackup
}