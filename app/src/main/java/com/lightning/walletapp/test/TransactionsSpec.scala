package com.lightning.walletapp.test

import java.nio.ByteOrder

import concurrent.ExecutionContext.Implicits.global
import com.lightning.walletapp.ln._
import com.lightning.walletapp.ln.Helpers.Funding
import com.lightning.walletapp.ln.Scripts._
import com.lightning.walletapp.ln.crypto.Generators._
import com.lightning.walletapp.ln.crypto.Sphinx
import com.lightning.walletapp.ln.wire.UpdateAddHtlc
import fr.acinq.bitcoin.Protocol.{One, Zeroes}
import fr.acinq.bitcoin._
import fr.acinq.bitcoin.Crypto._
import fr.acinq.bitcoin.Script._
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.io.Source
import scala.util.Random


class TransactionsSpec {
  def allTests = {

    {
      val htlcTx = Transaction.read("02000000014ae539d5b7967b50fc3071f5460927275d195e46868dc81a2cf400256572f3ca000000000000000000012a6b010000000000220020633a5f65a0247fa919f14ceab9ff5afd00cf9de5a36b09530d53facb9a43ea7300000000")
      val dustLimit = Satoshi(1100L)
      val remoteRevocationPubkey = PublicKey(ByteVector.fromValidHex("02fead8896083932987c1293a34826a5293abab9ead093280cb78283434a2767da"))
      val toSelfDelay = 144
      val remoteDelayedPaymentPubkey = PublicKey(ByteVector.fromValidHex("0221c61fca26c469d6e64ae22b10173767f4ac26a0eac3187e601ea9639c1ab960"))
      val defaultFinalScriptPubKey = ByteVector.fromValidHex("0014888dbbd7998f9f80d4f82b4f9ce6d5882f310aec")
      val feeratePerKwPenalty = 10000L
      val resultPenaltyTx = Transaction.read("0200000001fdefba99dc8708aff81e1479c454dac7d6e1eb689b44dee43915f4a4882e947c0000000000ffffffff013858010000000000160014888dbbd7998f9f80d4f82b4f9ce6d5882f310aec00000000")
      val singnedPenaltyTx = Transaction.read("02000000000101fdefba99dc8708aff81e1479c454dac7d6e1eb689b44dee43915f4a4882e947c0000000000ffffffff013858010000000000160014888dbbd7998f9f80d4f82b4f9ce6d5882f310aec0347304402204765cf132741a5c367b1a9de300f129595d7e03f64d9347ae9e83212caa8c09b0220414e891d3e896aaf29e4b0b89091d9d9d53d0d23ed7a5dc7391e1a36e5ddae3b0101014d632102fead8896083932987c1293a34826a5293abab9ead093280cb78283434a2767da67029000b275210221c61fca26c469d6e64ae22b10173767f4ac26a0eac3187e601ea9639c1ab96068ac00000000")
      val remotePerCommitmentSecret = Scalar(ByteVector.fromValidHex("4fedc052c833811ff298ebad5107911a394d0f87d0113554377f822b9f8b9417"))
      val remoteRevocationPrivkey = revocationPrivKey(PrivateKey(ByteVector.fromValidHex("e1d4a574e8757e0ce365410ff57f1cf82d37a63d3c87faad02bd85599bc935c101")), remotePerCommitmentSecret)

      val notSigned = Scripts.makeClaimDelayedOutputPenaltyTx(htlcTx,
        remoteRevocationPubkey, toSelfDelay, remoteDelayedPaymentPubkey,
        defaultFinalScriptPubKey, feeratePerKwPenalty,
        dustLimit).get

      val sig = Scripts.sign(remoteRevocationPrivkey)(notSigned)
      val signed = Scripts.addSigs(notSigned, revocationSig = sig)
      Transaction.correctlySpends(signed.tx, Seq(htlcTx), ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)
    }

    {
      println("encode/decode sequence and locktime (one example)")
      val txnumber = 0x11F71FB268DL

      val (sequence, locktime) = encodeTxNumber(txnumber)
      assert(sequence == 0x80011F71L)
      assert(locktime == 0x20FB268DL)

      val txnumber1 = decodeTxNumber(sequence, locktime)
      assert(txnumber == txnumber1)
    }

    {
      println("reconstruct txnumber from sequence and locktime")
      for (i <- 0 until 1000) {
        val txnumber = Random.nextLong() & 0xffffffffffffL
        val (sequence, locktime) = encodeTxNumber(txnumber)
        val txnumber1 = decodeTxNumber(sequence, locktime)
        assert(txnumber == txnumber1)
      }
    }

    {
      println("compute fees")
      // see BOLT #3 specs
      val htlcs = Set(
        Htlc(incoming = false, UpdateAddHtlc(ByteVector.fromValidHex("00" * 32), 0, MilliSatoshi(5000000).amount, Zeroes, 552)),
        Htlc(incoming = false, UpdateAddHtlc(ByteVector.fromValidHex("00" * 32), 0, MilliSatoshi(1000000).amount, Zeroes, 553)),
        Htlc(incoming = true, UpdateAddHtlc(ByteVector.fromValidHex("00" * 32), 0, MilliSatoshi(7000000).amount, Zeroes, 550)),
        Htlc(incoming = true, UpdateAddHtlc(ByteVector.fromValidHex("00" * 32), 0, MilliSatoshi(800000).amount, Zeroes, 551))
      )

      val spec = CommitmentSpec(feeratePerKw = 5000, toLocalMsat = 0, toRemoteMsat = 0, htlcs)
      val fee = Scripts.commitTxFee(Satoshi(546), spec)
      assert(fee == Satoshi(5340))
    }

    {
      println("check pre-computed transaction weights")
      val localRevocationPriv = PrivateKey(ByteVector.fromValidHex("cc" * 32), compressed = true)
      val localPaymentPriv = PrivateKey(ByteVector.fromValidHex("dd" * 32), compressed = true)
      val localHtlcPriv = PrivateKey(ByteVector.fromValidHex("ea" * 32), compressed = true)
      val remoteHtlcPriv = PrivateKey(ByteVector.fromValidHex("eb" * 32), compressed = true)
      val finalPubKeyScript = Script.write(Script.pay2wpkh(PrivateKey(ByteVector.fromValidHex("fe" * 32), compressed = true).publicKey))
      val localDustLimit = Satoshi(542)
      val toLocalDelay = 144
      val feeratePerKw = 1000

      {
        // ClaimP2WPKHOutputTx
        // first we create a fake commitTx tx, containing only the output that will be spent by the ClaimP2WPKHOutputTx
        val pubKeyScript = write(pay2wpkh(localPaymentPriv.publicKey))
        val commitTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(20000), pubKeyScript) :: Nil, lockTime = 0)
        val claimP2WPKHOutputTx = makeClaimP2WPKHOutputTx(commitTx, localPaymentPriv.publicKey, finalPubKeyScript, feeratePerKw, localDustLimit)
        // we use dummy signatures to compute the weight
        val weight = Transaction.weight(addSigs(claimP2WPKHOutputTx.get, ByteVector.fromValidHex("bb" * 71), localPaymentPriv.publicKey).tx)
        assert(claimP2WPKHOutputWeight == weight)
      }

      {
        // ClaimHtlcDelayedTx
        // first we create a fake htlcSuccessOrTimeoutTx tx, containing only the output that will be spent by the ClaimDelayedOutputTx
        val pubKeyScript = write(pay2wsh(toLocalDelayed(localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey)))
        val htlcSuccessOrTimeoutTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(20000), pubKeyScript) :: Nil, lockTime = 0)
        val claimHtlcDelayedTx = makeClaimDelayedOutputTx(htlcSuccessOrTimeoutTx, localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey, finalPubKeyScript, feeratePerKw, localDustLimit)
        // we use dummy signatures to compute the weight
        val weight = Transaction.weight(addSigs(claimHtlcDelayedTx.get, ByteVector.fromValidHex("bb" * 71)).tx)
        assert(claimHtlcDelayedWeight == weight)
      }

      {
        // MainPenaltyTx
        // first we create a fake commitTx tx, containing only the output that will be spent by the MainPenaltyTx
        val pubKeyScript = write(pay2wsh(toLocalDelayed(localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey)))
        val commitTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(20000), pubKeyScript) :: Nil, lockTime = 0)
        val mainPenaltyTx = makeMainPenaltyTx(commitTx, localRevocationPriv.publicKey, finalPubKeyScript, toLocalDelay, localPaymentPriv.publicKey, feeratePerKw, localDustLimit)
        // we use dummy signatures to compute the weight
        val weight = Transaction.weight(addSigs(mainPenaltyTx.get, ByteVector.fromValidHex("bb" * 71)).tx)
        assert(mainPenaltyWeight == weight)
      }

      {
        // HtlcPenaltyTx
        // first we create a fake commitTx tx, containing only the output that will be spent by the ClaimHtlcSuccessTx
        val paymentPreimage = ByteVector.fromValidHex("42" * 32)
        val htlc = UpdateAddHtlc(ByteVector.fromValidHex("00" * 32), 0, Satoshi(20000).amount * 1000, sha256(paymentPreimage), expiry = 400144)
        val redeemScript = htlcReceived(localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, localRevocationPriv.publicKey, ripemd160(htlc.paymentHash), htlc.expiry)
        val pubKeyScript = write(pay2wsh(redeemScript))
        val commitTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(htlc.amountMsat / 1000), pubKeyScript) :: Nil, lockTime = 0)
        val htlcPenaltyTx = makeHtlcPenaltyTx(new PubKeyScriptIndexFinder(commitTx), Script.write(redeemScript), finalPubKeyScript, feeratePerKw, localDustLimit)
        // we use dummy signatures to compute the weight
        val weight = Transaction.weight(addSigs(htlcPenaltyTx.get, ByteVector.fromValidHex("bb" * 71), localRevocationPriv.publicKey).tx)
        assert(htlcPenaltyWeight == weight)
      }

      {
        // ClaimHtlcSuccessTx
        // first we create a fake commitTx tx, containing only the output that will be spent by the ClaimHtlcSuccessTx
        val paymentPreimage = ByteVector.fromValidHex("42" * 32)
        val htlc = UpdateAddHtlc(ByteVector.fromValidHex("00" * 32), 0, Satoshi(20000).amount * 1000, sha256(paymentPreimage), expiry = 400144)
        val pubKeyScript = write(pay2wsh(htlcOffered(localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, localRevocationPriv.publicKey, ripemd160(htlc.paymentHash))))
        val commitTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(htlc.amountMsat / 1000), pubKeyScript) :: Nil, lockTime = 0)
        val claimHtlcSuccessTx = makeClaimHtlcSuccessTx(new PubKeyScriptIndexFinder(commitTx), remoteHtlcPriv.publicKey, localHtlcPriv.publicKey, localRevocationPriv.publicKey, finalPubKeyScript, htlc, feeratePerKw, localDustLimit)
        // we use dummy signatures to compute the weight
        val weight = Transaction.weight(addSigs(claimHtlcSuccessTx.get, ByteVector.fromValidHex("bb" * 71), paymentPreimage).tx)
        assert(claimHtlcSuccessWeight == weight)
      }

      {
        // ClaimHtlcTimeoutTx
        // first we create a fake commitTx tx, containing only the output that will be spent by the ClaimHtlcSuccessTx
        val paymentPreimage = ByteVector.fromValidHex("42" * 32)
        val htlc = UpdateAddHtlc(ByteVector.fromValidHex("00" * 32), 0, Satoshi(20000).amount * 1000, sha256(paymentPreimage), expiry = 400144)
        val pubKeyScript = write(pay2wsh(htlcReceived(localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, localRevocationPriv.publicKey, ripemd160(htlc.paymentHash), htlc.expiry)))
        val commitTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(htlc.amountMsat / 1000), pubKeyScript) :: Nil, lockTime = 0)
        val claimClaimHtlcTimeoutTx = makeClaimHtlcTimeoutTx(new PubKeyScriptIndexFinder(commitTx), remoteHtlcPriv.publicKey, localHtlcPriv.publicKey, localRevocationPriv.publicKey, finalPubKeyScript, htlc, feeratePerKw, localDustLimit)
        // we use dummy signatures to compute the weight
        val weight = Transaction.weight(addSigs(claimClaimHtlcTimeoutTx.get, ByteVector.fromValidHex("bb" * 71)).tx)
        assert(claimHtlcTimeoutWeight == weight)
      }
    }

    {
      println("generate valid commitment and htlc transactions")
      val localFundingPriv = PrivateKey(ByteVector.fromValidHex("a1" * 32) :+ 1.toByte)
      val remoteFundingPriv = PrivateKey(ByteVector.fromValidHex("a2" * 32) :+ 1.toByte)
      val localRevocationPriv = PrivateKey(ByteVector.fromValidHex("a3" * 32) :+ 1.toByte)
      val localPaymentPriv = PrivateKey(ByteVector.fromValidHex("a4" * 32) :+ 1.toByte)
      val localDelayedPaymentPriv = PrivateKey(ByteVector.fromValidHex("a5" * 32) :+ 1.toByte)
      val remotePaymentPriv = PrivateKey(ByteVector.fromValidHex("a6" * 32) :+ 1.toByte)
      val localHtlcPriv = PrivateKey(ByteVector.fromValidHex("a7" * 32) :+ 1.toByte)
      val remoteHtlcPriv = PrivateKey(ByteVector.fromValidHex("a8" * 32) :+ 1.toByte)
      val finalPubKeyScript = Script.write(Script.pay2wpkh(PrivateKey(ByteVector.fromValidHex("a9" * 32), compressed = true).publicKey))
      val commitInput = Funding.makeFundingInputInfo(ByteVector.fromValidHex("a0" * 32), 0, Btc(1), localFundingPriv.publicKey, remoteFundingPriv.publicKey)
      val toLocalDelay = 144
      val localDustLimit = Satoshi(542)
      val feeratePerKw = 22000


      // htlc1 and htlc2 are regular IN/OUT htlcs
      val paymentPreimage1 = ByteVector.fromValidHex("11" * 32)
      val htlc1 = UpdateAddHtlc(ByteVector.fromValidHex("00" * 32), 0, millibtc2satoshi(MilliBtc(100)).amount * 1000, sha256(paymentPreimage1), 300)
      val paymentPreimage2 = ByteVector.fromValidHex("22" * 32)
      val htlc2 = UpdateAddHtlc(ByteVector.fromValidHex("00" * 32), 1, millibtc2satoshi(MilliBtc(200)).amount * 1000, sha256(paymentPreimage2), 300)
      // htlc3 and htlc4 are dust htlcs IN/OUT htlcs, with an amount large enough to be included in the commit tx, but too small to be claimed at 2nd stage
      val paymentPreimage3 = ByteVector.fromValidHex("33" * 32)
      val htlc3 = UpdateAddHtlc(ByteVector.fromValidHex("00" * 32), 2, (localDustLimit + weight2fee(feeratePerKw, htlcTimeoutWeight)).amount * 1000, sha256(paymentPreimage3), 300)
      val paymentPreimage4 = ByteVector.fromValidHex("44" * 32)
      val htlc4 = UpdateAddHtlc(ByteVector.fromValidHex("00" * 32), 3, (localDustLimit + weight2fee(feeratePerKw, htlcSuccessWeight)).amount * 1000, sha256(paymentPreimage4), 300)
      val spec = CommitmentSpec(
        feeratePerKw = feeratePerKw,
        toLocalMsat = millibtc2satoshi(MilliBtc(400)).amount * 1000,
        toRemoteMsat = millibtc2satoshi(MilliBtc(300)).amount * 1000,
        htlcs = Set(
          Htlc(incoming = false, htlc1),
          Htlc(incoming = true, htlc2),
          Htlc(incoming = false, htlc3),
          Htlc(incoming = true, htlc4)
        ))

      val commitTxNumber = 0x404142434445L
      val commitTx = {
        val txinfo = makeCommitTx(commitInput, commitTxNumber, localPaymentPriv.toPoint, remotePaymentPriv.toPoint, true, localDustLimit, localRevocationPriv.publicKey, toLocalDelay, localDelayedPaymentPriv.publicKey, remotePaymentPriv.publicKey, localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, spec)
        val localSig = Scripts.sign(localPaymentPriv)(txinfo)
        val remoteSig = Scripts.sign(remotePaymentPriv)(txinfo)
        Scripts.addSigs(txinfo, localFundingPriv.publicKey, remoteFundingPriv.publicKey, localSig, remoteSig)
      }

      {
        assert(getCommitTxNumber(commitTx.tx, true, localPaymentPriv.publicKey, remotePaymentPriv.publicKey) == commitTxNumber)
        val hash = Crypto.sha256(localPaymentPriv.publicKey.toBin ++ remotePaymentPriv.publicKey.toBin)
        val num = Protocol.uint64(hash.takeRight(8).toArray, ByteOrder.BIG_ENDIAN) & 0xffffffffffffL
        val check = ((commitTx.tx.txIn(0).sequence & 0xffffff) << 24) | commitTx.tx.lockTime
        assert((check ^ num) == commitTxNumber)
      }
      val (htlcTimeoutTxs, htlcSuccessTxs) = makeHtlcTxs(commitTx.tx, localDustLimit, localRevocationPriv.publicKey, toLocalDelay, localDelayedPaymentPriv.publicKey, localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, spec)

      assert(htlcTimeoutTxs.size == 2) // htlc1 and htlc3
      assert(htlcSuccessTxs.size == 2) // htlc2 and htlc4

      {
        // either party spends local->remote htlc output with htlc timeout tx
        for (htlcTimeoutTx <- htlcTimeoutTxs) {
          val localSig = Scripts.sign(localHtlcPriv)(htlcTimeoutTx)
          val remoteSig = Scripts.sign(remoteHtlcPriv)(htlcTimeoutTx)
          val signed = addSigs(htlcTimeoutTx, localSig, remoteSig)
          assert(checkValid(signed).isSuccess)
        }
      }

      {
        // remote spends local->remote htlc1/htlc3 output directly in case of success
        for ((htlc, paymentPreimage) <- (htlc1, paymentPreimage1) :: (htlc3, paymentPreimage3) :: Nil) {
          val claimHtlcSuccessTx = makeClaimHtlcSuccessTx(new PubKeyScriptIndexFinder(commitTx.tx), remoteHtlcPriv.publicKey, localHtlcPriv.publicKey, localRevocationPriv.publicKey, finalPubKeyScript, htlc, feeratePerKw, localDustLimit).get
          val localSig = Scripts.sign(remoteHtlcPriv)(claimHtlcSuccessTx)
          val signed = addSigs(claimHtlcSuccessTx, localSig, paymentPreimage)
          assert(checkValid(signed).isSuccess)
        }
      }

      {
        // local spends remote->local htlc2/htlc4 output with htlc success tx using payment preimage
        for ((htlcSuccessTx, paymentPreimage) <- (htlcSuccessTxs(0), paymentPreimage2) :: (htlcSuccessTxs(1), paymentPreimage4) :: Nil) {
          val localSig = Scripts.sign(localHtlcPriv)(htlcSuccessTx)
          val remoteSig = Scripts.sign(remoteHtlcPriv)(htlcSuccessTx)
          val signedTx = addSigs(htlcSuccessTx, localSig, remoteSig, paymentPreimage)
          assert(checkValid(signedTx).isSuccess)
          // check remote sig
          assert(checkSig(htlcSuccessTx, remoteSig, remoteHtlcPriv.publicKey))
        }
      }

      {
        // remote spends main output
        val claimP2WPKHOutputTx = makeClaimP2WPKHOutputTx(commitTx.tx, remotePaymentPriv.publicKey, finalPubKeyScript, feeratePerKw, localDustLimit).get
        val localSig = Scripts.sign(remotePaymentPriv)(claimP2WPKHOutputTx)
        val signedTx = addSigs(claimP2WPKHOutputTx, localSig, remotePaymentPriv.publicKey)
        assert(checkValid(signedTx).isSuccess)
      }

      {
        // remote spends remote->local htlc output directly in case of timeout
        val claimHtlcTimeoutTx = makeClaimHtlcTimeoutTx(new PubKeyScriptIndexFinder(commitTx.tx), remoteHtlcPriv.publicKey, localHtlcPriv.publicKey, localRevocationPriv.publicKey, finalPubKeyScript, htlc2, feeratePerKw, localDustLimit).get
        val localSig = Scripts.sign(remoteHtlcPriv)(claimHtlcTimeoutTx)
        val signed = addSigs(claimHtlcTimeoutTx, localSig)
        assert(checkValid(signed).isSuccess)
      }

      {
        // remote spends offered HTLC output with revocation key
        val script = Script.write(Scripts.htlcOffered(localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, localRevocationPriv.publicKey, Crypto.ripemd160(htlc1.paymentHash)))
        val htlcPenaltyTx = makeHtlcPenaltyTx(new PubKeyScriptIndexFinder(commitTx.tx), script, finalPubKeyScript, feeratePerKw, localDustLimit).get
        val sig = Scripts.sign(localRevocationPriv)(htlcPenaltyTx)
        val signed = addSigs(htlcPenaltyTx, sig, localRevocationPriv.publicKey)
        assert(checkValid(signed).isSuccess)
      }

      {
        // remote spends received HTLC output with revocation key
        val script = Script.write(Scripts.htlcReceived(localHtlcPriv.publicKey, remoteHtlcPriv.publicKey, localRevocationPriv.publicKey, Crypto.ripemd160(htlc2.paymentHash), htlc2.expiry))
        val htlcPenaltyTx = makeHtlcPenaltyTx(new PubKeyScriptIndexFinder(commitTx.tx), script, finalPubKeyScript, feeratePerKw, localDustLimit).get
        val sig = Scripts.sign(localRevocationPriv)(htlcPenaltyTx)
        val signed = addSigs(htlcPenaltyTx, sig, localRevocationPriv.publicKey)
        assert(checkValid(signed).isSuccess)
      }

    }

    def htlc(incoming: Boolean, amount: Satoshi): Htlc =
      Htlc(incoming, UpdateAddHtlc(ByteVector.fromValidHex("00" * 32), 0, amount.amount * 1000, ByteVector.fromValidHex("00" * 32), 144))

    Future {
      println("BOLT 2 fee tests")

      val bolt3 = Source
        .fromURL("https://raw.githubusercontent.com/lightningnetwork/lightning-rfc/master/03-transactions.md")
        .mkString
        .replace("    name:", "$   name:")
      // character '$' separates tests

      // this regex extract params from a given test
      val testRegex = ("""name: (.*)\n""" +
        """.*to_local_msat: ([0-9]+)\n""" +
        """.*to_remote_msat: ([0-9]+)\n""" +
        """.*feerate_per_kw: ([0-9]+)\n""" +
        """.*base commitment transaction fee = ([0-9]+)\n""" +
        """[^$]+""").r
      // this regex extracts htlc direction and amounts
      val htlcRegex =
      """.*HTLC ([a-z]+) amount ([0-9]+).*""".r

      val dustLimit = Satoshi(546)
      case class TestSetup(name: String, dustLimit: Satoshi, spec: CommitmentSpec, expectedFee: Satoshi)

      val tests = testRegex.findAllIn(bolt3).map(s => {
        val testRegex(name, to_local_msat, to_remote_msat, feerate_per_kw, fee) = s
        val htlcs = htlcRegex.findAllIn(s).map(l => {
          val htlcRegex(direction, amount) = l
          direction match {
            case "offered" => htlc(incoming = false, Satoshi(amount.toLong))
            case "received" => htlc(incoming = true, Satoshi(amount.toLong))
          }
        }).toSet
        TestSetup(name, dustLimit, CommitmentSpec(feerate_per_kw.toLong, to_local_msat.toLong, to_remote_msat.toLong, htlcs), Satoshi(fee.toLong))
      })

      // simple non-reg test making sure we are not missing tests
      println(tests.size == 15, "there were 15 tests at ec99f893f320e8c88f564c1c8566f3454f0f1f5f")

      tests.foreach(test => {
        println(s"running BOLT 2 test: '${test.name}'")
        val fee = commitTxFee(test.dustLimit, test.spec)
        assert(fee == test.expectedFee)
      })

      println("SUCCESS")
    }
  }
}