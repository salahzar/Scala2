package com.pakkio.binary

import org.scalatest.FunSuite

import Implicits._




class DecipherTest extends FunSuite {


  private val KEY = "aaaa"
  private val EXTENSIONLEN = 100*KEY.length




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
