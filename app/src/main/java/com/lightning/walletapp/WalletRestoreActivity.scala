package com.lightning.walletapp

import android.widget._
import android.widget.DatePicker._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.R.string._
import com.hootsuite.nachos.terminator.ChipTerminatorHandler._
import com.lightning.walletapp.lnutils.{ChannelWrap, LocalBackup}
import com.lightning.walletapp.ln.Tools.{none, runAnd, wrap}
import org.bitcoinj.wallet.{DeterministicSeed, Wallet}
import scala.util.{Failure, Success, Try}
import android.view.{View, ViewGroup}

import com.hootsuite.nachos.NachoTextView
import com.lightning.walletapp.Utils.app
import org.bitcoinj.crypto.MnemonicCode
import java.util.Calendar
import android.os.Bundle


class WhenPicker(host: TimerActivity, start: Long) extends DatePicker(host) with OnDateChangedListener { me =>
  def humanTime = java.text.DateFormat getDateInstance java.text.DateFormat.MEDIUM format cal.getTime
  def onDateChanged(view: DatePicker, year: Int, mon: Int, date: Int) = cal.set(year, mon, date)
  def refresh = runAnd(me)(try getParent.asInstanceOf[ViewGroup] removeView me catch none)
  init(cal get Calendar.YEAR, cal get Calendar.MONTH, cal get Calendar.DATE, me)

  lazy val cal = {
    val calendar = Calendar.getInstance
    calendar setTimeInMillis start
    me setMinDate start
    calendar
  }
}

class WalletRestoreActivity extends TimerActivity with FirstActivity { me =>
  lazy val restoreCode = findViewById(R.id.restoreCode).asInstanceOf[NachoTextView]
  lazy val restoreProgress = findViewById(R.id.restoreProgress).asInstanceOf[View]
  lazy val restoreWallet = findViewById(R.id.restoreWallet).asInstanceOf[Button]
  lazy val restoreWhen = findViewById(R.id.restoreWhen).asInstanceOf[Button]
  lazy val restoreInfo = findViewById(R.id.restoreInfo).asInstanceOf[View]
  lazy val dp = new WhenPicker(me, 1526817600 * 1000L)

  def INIT(state: Bundle) = {
    setContentView(R.layout.activity_restore)
    restoreCode addTextChangedListener new TextChangedWatcher {
      def isMnemonicCorrect = getMnemonic.split("\\s+").length > 11
      override def onTextChanged(c: CharSequence, x: Int, y: Int, z: Int) = {
        val txt = if (isMnemonicCorrect) wallet_restore else restore_mnemonic_wrong
        restoreWallet setEnabled isMnemonicCorrect
        restoreWallet setText txt
      }
    }

    restoreWhen setText dp.humanTime
    restoreCode.addChipTerminator(' ', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode.addChipTerminator(',', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode.addChipTerminator('\n', BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    restoreCode setDropDownBackgroundResource R.color.button_material_dark
    restoreCode setAdapter new ArrayAdapter(me, android.R.layout.simple_list_item_1, MnemonicCode.INSTANCE.getWordList)

    val backupAllowed = LocalBackup.isAllowed(activity = me)
    if (!backupAllowed) LocalBackup.askPermission(activity = me)
    else onRequestPermissionsResult(100, Array.empty, Array.empty)
  }

  override def onBackPressed: Unit = wrap(super.onBackPressed)(app.kit.stopAsync)
  def getMnemonic: String = restoreCode.getText.toString.trim.toLowerCase.replaceAll("[^a-zA-Z0-9']+", " ")
  def setWhen(btn: View) = mkCheckForm(alert => rm(alert)(restoreWhen setText dp.humanTime), none, baseBuilder(null, dp.refresh), dialog_ok, dialog_cancel)

  def recWallet(top: View) =
    app.kit = new app.WalletKit {
      restoreProgress setVisibility View.VISIBLE
      restoreInfo setVisibility View.GONE
      startAsync

      def startUp = {
        // Make sure wallet is fully set up before attempting to restore from backup
        val seed = new DeterministicSeed(getMnemonic, null, "", dp.cal.getTimeInMillis / 1000)
        LNParams setup seed.getSeedBytes

        Try {
          val dir = LocalBackup.getBackupDirectory(LNParams.chainHash)
          LocalBackup.getBackupFileUnsafe(dir, LNParams.keys.cloudId)
        } match {
          case Success(backupFile) if backupFile.isFile =>
            // Backup file is present, decode it and restore chans if successful
            LocalBackup.readAndDecrypt(backupFile, LNParams.keys.cloudSecret) match {
              // Decoding may still fail, do not proceed and inform user in that case

              case Success(localBackups) =>
                // Update ealiest key creation time to our watch timestamp
                seed.setCreationTimeSeconds(localBackups.earliestUtxoSeconds)
                wallet = Wallet.fromSeed(app.params, seed)

                // Restore channels before proceeding
                localBackups.hosted.foreach(restoreHostedChannel)
                localBackups.normal.foreach(restoreNormalChannel)
                me prepareFreshWallet app.kit

              case Failure(reason) => UITask {
                val message = getString(ln_decrypt_fail).format(reason.getMessage)
                showForm(negTextBuilder(dialog_ok, msg = message).create)
                restoreProgress setVisibility View.GONE
                restoreInfo setVisibility View.VISIBLE
              }.run
            }

          case _ =>
            // Not allowed by user or not present at all
            wallet = Wallet.fromSeed(app.params, seed)
            me prepareFreshWallet app.kit
        }
      }
    }

  // Restoring from local backup

  def restoreHostedChannel(some: HostedCommits) = {
    val chan = ChannelManager.createHostedChannel(ChannelManager.operationalListeners, some)
    // Do not use STORE because it invokes a backup saving while we already have it
    ChannelManager.all :+= chan
    ChannelWrap put some
  }

  def restoreNormalChannel(some: HasNormalCommits) = some match {
    case closing: ClosingData => restoreClosedNormalChannel(closing)
    case _ => restoreNotClosedNormalChannel(some)
  }

  def restoreClosedNormalChannel(cd: ClosingData) = {
    val chan = ChannelManager.createChannel(ChannelManager.operationalListeners, cd)
    // Watch future commit spends and check if any was published while we were offline
    app.kit.wallet.addWatchedScripts(app.kit closingPubKeyScripts cd)
    ChannelManager.check2ndLevelSpent(chan, cd)
    // Do not STORE because it invokes backup
    ChannelManager.all :+= chan
    ChannelWrap put cd
  }

  def restoreNotClosedNormalChannel(some: HasNormalCommits) = {
    val chan = ChannelManager.createChannel(ChannelManager.operationalListeners, some)
    // Watch for future channel spends and check if any was published while we were offline
    app.kit.wallet.addWatchedScripts(app.kit fundingPubScript some)
    ChannelManager.check1stLevelSpent(chan, some)
    // Do not STORE because it invokes backup
    ChannelManager.all :+= chan
    ChannelWrap put some
  }
}