package com.pakkio.binary

import org.scalatest.FunSuite

import Implicits._




class DecipherTest extends FunSuite {

  val m1:Buffer="the first thing to think when we are ready to go out is to be happy"
  val m2:Buffer="somebody can tell you that we are not good to do the good, but we are sure we can do that"
  val m3:Buffer="this is the third and last message do try to decrypt"
  val m4:Buffer="a story in the world is where you can learn from your errors"
  val m5:Buffer="similarities are often casuals and cannot be reproduced"
  val m6:Buffer="some people say the same as other people, but we cannot guarantee"
  private val KEY = "aaaa"
  private val EXTENSIONLEN = 100*KEY.length

  test("crypting three messages with same KEY") {
    val c1 = m1.crypt(KEY)
    val c2 = m2.crypt(KEY)
    val c3 = m3.crypt(KEY)
    val c4 = m4.crypt(KEY)
    val c5 = m5.crypt(KEY)
    val c6 = m6.crypt(KEY)

    val c1c2=c1.xor(c2)
    val c1c3=c1.xor(c3)
    val c2c3=c2.xor(c3)
    val c1c4=c1.xor(c4)
    val c1c5=c1.xor(c5)
    val c1c6=c1.xor(c6)

    val matching1=matching(
      List(c1c2.findSpaces(),c1c3.findSpaces(),c1c4.findSpaces(),c1c5.findSpaces()))
    println(s"m1: \n${m1.toString} spaces: \n$matching1")

  }


  test("extend ascii working correctly") {
    val trivial = TrivialExtend(KEY)
    val f = trivial.extend(EXTENSIONLEN)
    println(s"extend ascii: ${f.toHex} randomness: ${f.analyze}")
    val repetitions = EXTENSIONLEN / KEY.length
    assert(f.toHex === (KEY.toHex * repetitions))
    println(s"extend ascii randomness: ${f.analyze}")

  }
  test("extend crypt working, producing the same extended " + KEY + " if called twice with the same " + KEY) {
    // this guarantees a longer key with muc more randomness than just
    // repeating
    val advanced = CryptoExtend(KEY)
    val f = advanced.extend(EXTENSIONLEN)
    val advanced2 = CryptoExtend(KEY)
    val f2 = advanced2.extend(EXTENSIONLEN)

    println(s"extend crypto1: ${f.toHex} \nrandomness: ${f.analyze}")

    assert(f.toHex === f2.toHex)
  }

}
