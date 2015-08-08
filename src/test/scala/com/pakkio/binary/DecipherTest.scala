package com.pakkio.binary

import org.scalatest.FunSuite


class DecipherTest extends FunSuite with Implicits {
  type BA = Array[Byte]

  class CExtend(seed: BA) {

    import java.security.SecureRandom

    val sr = new SecureRandom(seed)

  }

  def crypt(m: Buffer, k: Buffer) = {
    m.xor(k)
  }

  /**
   * Extends key until a specified length
   * repeating it
   * @param key the original key to extend
   * @param l the final desired length
   * @extendFunction is the function extending, default is identity i.e. replicating
   * @return
   */
  def extend(key: Buffer,
             l: Int,
             function: CryptoExtendable)
  : Buffer = {
    var ret = new Array[Byte](0)
    while (ret.length < l) ret = ret ++ function.next(key.content)
    Buffer(ret.slice(0, l))

  }

  abstract class CryptoExtendable(seed: BA) {
    def next(x: BA): BA
  }

  case class CryptoExtend(seed: BA) extends CryptoExtendable(seed) {

    import java.security.SecureRandom

    val generator = new SecureRandom(seed)

    override def next(x: BA) = {
      var ret = new Array[Byte](seed.length)
      generator.nextBytes(ret)
      ret
    }

  }

  case class TrivialExtend(seed: BA) extends CryptoExtendable(seed) {
    override def next(x: BA) = seed
  }


  private val KEY = "asD$%"
  private val EXTENSIONLEN = 100*KEY.length

  test("setting a crypted message with same " + KEY) {
    val c1 = crypt("alle falde", KEY)
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
