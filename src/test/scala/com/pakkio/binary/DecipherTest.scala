package com.pakkio.binary

import org.scalatest.FunSuite

import Implicits._

class DecipherTest extends FunSuite {



  private val KEY = "asD$%"
  private val EXTENSIONLEN = 100*KEY.length

  test("setting a crypted message with same " + KEY) {
    val c1 = Buffer("alle falde").crypt(KEY)
    println(c1.toHex)

  }

  def analyze(content: Array[Byte]) = {
    val ret = content.groupBy(_.toByte)
    val mapped = ret.map(p=>p._2.length)
    val avg=(0.0 /: mapped){ _+_ } / mapped.size
    avg
  }

  test("extend ascii working correctly") {
    val f = extend(KEY, EXTENSIONLEN, TrivialExtend(KEY))
    println(s"extend ascii: ${f.toHex} randomness: ${analyze(f.content)}")
    val repetitions = EXTENSIONLEN / KEY.length
    assert(f.toHex === (Buffer(KEY).toHex * repetitions))
    println("extend ascii randomness: " + analyze(f.content))

  }
  test("extend crypt working, producing the same extended " + KEY + " if called twice with the same " + KEY) {
    // this guarantees a longer key with muc more randomness than just
    // repeating
    val cryptoKey = CryptoExtend(KEY)
    val f = extend(KEY, EXTENSIONLEN, cryptoKey)
    val cryptoKey2 = CryptoExtend(KEY)
    val f2 = extend(KEY, EXTENSIONLEN, cryptoKey2)

    println(s"extend crypto1: ${f.toHex} \nrandomness: ${analyze(f.content)}")

    assert(f.toHex === f2.toHex)
  }

}
