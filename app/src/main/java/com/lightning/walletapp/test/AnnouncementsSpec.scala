package com.lightning.walletapp.test

import com.lightning.walletapp.ln.Announcements._
import fr.acinq.bitcoin.Block
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import scodec.bits.ByteVector

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random


class AnnouncementsSpec {
  def randomKey: PrivateKey = PrivateKey(ByteVector{
    val bin = Array.fill[Byte](32)(0)
    Random.nextBytes(bin)
    bin
  }, compressed = true)

  val alicePk = PrivateKey(ByteVector.fromValidHex("cc" * 32), compressed = true)

  def allTests = Future {

    {
      println("check nodeId1/nodeId2 lexical ordering")
      val node1 = PublicKey(ByteVector fromValidHex "027710df7a1d7ad02e3572841a829d141d9f56b17de9ea124d2f83ea687b2e0461")
      val node2 = PublicKey(ByteVector fromValidHex "0306a730778d55deec162a74409e006034a24c46d541c67c6c45f89a2adde3d9b4")
      // NB: node1 < node2
      assert(isNode1(node1, node2))
      assert(!isNode1(node2, node1))
    }

    {
      println("create valid signed channel announcement")
      val (node_a, node_b, bitcoin_a, bitcoin_b) = (randomKey, randomKey, randomKey, randomKey)
      val (node_a_sig, bitcoin_a_sig) = signChannelAnnouncement(Block.RegtestGenesisBlock.hash, 42, node_a, node_b.publicKey, bitcoin_a, bitcoin_b.publicKey, ByteVector.empty)
      val (node_b_sig, bitcoin_b_sig) = signChannelAnnouncement(Block.RegtestGenesisBlock.hash, 42, node_b, node_a.publicKey, bitcoin_b, bitcoin_a.publicKey, ByteVector.empty)
      val ann = makeChannelAnnouncement(Block.RegtestGenesisBlock.hash, 42, node_a.publicKey, node_b.publicKey, bitcoin_a.publicKey, bitcoin_b.publicKey, node_a_sig, node_b_sig, bitcoin_a_sig, bitcoin_b_sig)
      assert(checkSigs(ann))
      assert(!checkSigs(ann.copy(nodeId1 = randomKey.publicKey)))
    }

    {
      println("create valid signed channel update announcement")
      val ann = makeChannelUpdate(Block.RegtestGenesisBlock.hash, alicePk, randomKey.publicKey, 45561, 10, 10000, 100, 1, 500000000L)
      assert(checkSig(ann, alicePk.publicKey))
      assert(!checkSig(ann, randomKey.publicKey))
    }

    {
      println("check flags")
      val node1_priv = PrivateKey(ByteVector fromValidHex "5f447b05d86de82de6b245a65359d22f844ae764e2ae3824ac4ace7d8e1c749b01")
      val node2_priv = PrivateKey(ByteVector fromValidHex "eff467c5b601fdcc07315933767013002cd0705223d8e526cbb0c1bc75ccb62901")
      // NB: node1 < node2 (public keys)
      assert(isNode1(node1_priv.publicKey, node2_priv.publicKey))
      assert(!isNode1(node2_priv.publicKey, node1_priv.publicKey))
      val channelUpdate1 = makeChannelUpdate(Block.RegtestGenesisBlock.hash, node1_priv, node2_priv.publicKey, 0, 0, 0, 0, 0, 500000000L, enable = true)
      val channelUpdate1_disabled = makeChannelUpdate(Block.RegtestGenesisBlock.hash, node1_priv, node2_priv.publicKey, 0, 0, 0, 0, 0, 500000000L, enable = false)
      val channelUpdate2 = makeChannelUpdate(Block.RegtestGenesisBlock.hash, node2_priv, node1_priv.publicKey, 0, 0, 0, 0, 0, 500000000L, enable = true)
      val channelUpdate2_disabled = makeChannelUpdate(Block.RegtestGenesisBlock.hash, node2_priv, node1_priv.publicKey, 0, 0, 0, 0, 0, 500000000L, enable = false)
      assert(channelUpdate1.channelFlags == 0) // ....00
      assert(channelUpdate1_disabled.channelFlags == 2) // ....10
      assert(channelUpdate2.channelFlags == 1) // ....01
      assert(channelUpdate2_disabled.channelFlags == 3) // ....11
      assert(isNode1(channelUpdate1.channelFlags))
      assert(isNode1(channelUpdate1_disabled.channelFlags))
      assert(!isNode1(channelUpdate2.channelFlags))
      assert(!isNode1(channelUpdate2_disabled.channelFlags))
      assert(isEnabled(channelUpdate1.channelFlags))
      assert(!isEnabled(channelUpdate1_disabled.channelFlags))
      assert(isEnabled(channelUpdate2.channelFlags))
      assert(!isEnabled(channelUpdate2_disabled.channelFlags))
    }

  }
}
