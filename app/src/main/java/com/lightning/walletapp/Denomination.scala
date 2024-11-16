package com.lightning.walletapp

import R.string._
import java.text._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.Denomination.reddish
import com.lightning.walletapp.Utils.app
import android.content.res.Configuration
import fr.acinq.bitcoin.MilliSatoshi
import language.implicitConversions
import org.bitcoinj.core.Coin
import language.postfixOps


object Denomination {
  val locale = new java.util.Locale("en", "US")
  val symbols = new DecimalFormatSymbols(locale)
  symbols.setGroupingSeparator('\u00A0')
  symbols.setDecimalSeparator('.')

  // TODO: remove this hack once themed color can be appropriately fetched
  val mode = app.getResources.getConfiguration.uiMode & Configuration.UI_MODE_NIGHT_MASK
  val yellowHighlight = if (mode == Configuration.UI_MODE_NIGHT_YES) 0xFF444133 else 0xFFFFFAE4
  val reddish = if (mode == Configuration.UI_MODE_NIGHT_YES) "#E35646" else "#E31300"

  val formatFiat = new DecimalFormat("#,###,###.##")
  formatFiat setDecimalFormatSymbols symbols

  def btcBigDecimal2MSat(btc: BigDecimal) = MilliSatoshi(btc * BtcDenomination.factor toLong)
  implicit def mSat2Coin(msat: MilliSatoshi): Coin = Coin.valueOf(msat.amount / 1000L)
  implicit def coin2MSat(cn: Coin): MilliSatoshi = MilliSatoshi(cn.value * 1000L)
}

trait Denomination { me =>
  def rawString2MSat(raw: String) = MilliSatoshi(BigDecimal(raw) * factor toLong)
  def asString(msat: MilliSatoshi) = fmt format BigDecimal(msat.amount) / factor
  def parsedWithSign(msat: MilliSatoshi) = parsed(msat) + sign
  protected def parsed(msat: MilliSatoshi): String

  def coloredP2WSH(msat: MilliSatoshi, suffix: String): String = {
    val content = s"<font color=#0099FE>${this parsed msat}</font>$suffix"
    val start = "<tt><font color=#AAAAAA>[</font></tt>"
    val end = "<tt><font color=#AAAAAA>]</font></tt>"
    s"$start$content$end"
  }

  def coloredOut(msat: MilliSatoshi, suffix: String) = s"<font color=$reddish><tt>-</tt>${me parsed msat}</font>$suffix"
  def coloredIn(msat: MilliSatoshi, suffix: String) = s"<font color=#6AAB38><tt>+</tt>${me parsed msat}</font>$suffix"

  val amountInTxt: String
  val fmt: DecimalFormat
  val factor: Long
  val sign: String
}

object SatDenomination extends Denomination {
  val fmt = new DecimalFormat("###,###,###.###")
  val amountInTxt = app getString amount_hint_sat
  val sign = "\u00A0sat"
  val factor = 1000L

  fmt setDecimalFormatSymbols symbols
  def parsed(msat: MilliSatoshi) = {
    val basicFormattedSum = asString(msat)
    val dotIndex = basicFormattedSum indexOf "."
    val whole \ decimal = basicFormattedSum splitAt dotIndex
    if (decimal == basicFormattedSum) basicFormattedSum
    else s"$whole<small>$decimal</small>"
  }
}

object BtcDenomination extends Denomination {
  val fmt = new DecimalFormat("##0.00000000###")
  val amountInTxt = app getString amount_hint_btc
  val factor = 100000000000L
  val sign = "\u00A0btc"

  fmt setDecimalFormatSymbols symbols
  def parsed(msat: MilliSatoshi) =
    asString(msat) take 10
}