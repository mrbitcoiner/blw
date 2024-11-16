package com.lightning.walletapp.ln.crypto

import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.Crypto.{Scalar, Point}
import fr.acinq.bitcoin.Crypto
import ShaChain.largestTxIndex
import scodec.bits.ByteVector


object Generators {
  def perCommitSecret(seed: ByteVector, index: Long): Scalar = {
    val result = ShaChain.shaChainFromSeed(seed.toArray, largestTxIndex - index)
    ByteVector.view(result)
  }

  def perCommitPoint(seed: ByteVector, index: Long): Point =
    perCommitSecret(seed, index).toPoint

  def points2Scalar(point1: Point, point2: Point) = {
    val result = point1.toBin(true) ++ point2.toBin(true)
    Scalar(Crypto sha256 result)
  }

  def derivePrivKey(secret: Scalar, perCommitPoint: Point) = {
    val scalar = secret add points2Scalar(perCommitPoint, secret.toPoint)
    PrivateKey(scalar)
  }

  def derivePubKey(basePoint: Point, perCommitPoint: Point) = {
    val point = basePoint add points2Scalar(perCommitPoint, basePoint).toPoint
    PublicKey(point)
  }

  def revocationPubKey(basePoint: Point, perCommitPoint: Point) = {
    val a = basePoint multiply points2Scalar(basePoint, perCommitPoint)
    val b = perCommitPoint multiply points2Scalar(perCommitPoint, basePoint)
    PublicKey(a add b)
  }

  def revocationPrivKey(secret: Scalar, perCommitSecret: Scalar) = {
    val a = secret multiply points2Scalar(secret.toPoint, perCommitSecret.toPoint)
    val b = perCommitSecret multiply points2Scalar(perCommitSecret.toPoint, secret.toPoint)
    PrivateKey(a add b)
  }
}