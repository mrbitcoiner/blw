package com.lightning.walletapp.test

import com.lightning.walletapp.ln.crypto.Hmac256
import fr.acinq.bitcoin.Protocol
import scodec.bits.ByteVector


class MacSpec {

  def allTests = {
    {
      println("HMAC-256 mac/verify")
      val keys = Seq(
        ByteVector.fromValidHex("0000000000000000000000000000000000000000000000000000000000000000"),
        ByteVector.fromValidHex("eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619"),
        ByteVector.fromValidHex("24653eac434488002cc06bbfb7f10fe18991e35f9fe4302dbea6d2353dc0ab1c7f31ebc5462c1fdce1b737ecff52d37d75dea43ce11c74d25aa297165faa2007")
      )
      val messages = Seq(
        ByteVector.fromValidHex("2a"),
        ByteVector.fromValidHex("451"),
        ByteVector.fromValidHex("eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619"),
        ByteVector.fromValidHex("fd0001000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff")
      )

      for (key <- keys) {
        val instance = Hmac256(key)
        for (message <- messages) {
          assert(instance.verify(instance.mac(message), message))
        }
      }
    }

    {
      println("HMAC-256 invalid macs")
      val instance = Hmac256(Protocol.Zeroes)
      val testCases = Seq(
        (ByteVector.fromValidHex("0000000000000000000000000000000000000000000000000000000000000000"), ByteVector.fromValidHex("2a")),
        (ByteVector.fromValidHex("4aa79e2da0cb5beae9b5dad4006909cb402e4201e191733bc2b5279629e4ed80"), ByteVector.fromValidHex("fd0001000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f30313233"))
      ).map(testCase => (testCase._1, testCase._2))

      for ((mac, message) <- testCases) {
        assert(!instance.verify(mac, message))
      }
    }
  }
}
