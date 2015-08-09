package com.pakkio.binary

import Implicits._

abstract class CryptoExtendable(seed: BA)  {


  def extend(l: Int)
  : Buffer = {
    var ret = new Array[Byte](0)
    while (ret.length < l) ret = ret ++ next
    Buffer(ret.slice(0, l))

  }

  def next: BA
}

case class CryptoExtend(seed: BA) extends CryptoExtendable(seed) {

  import java.security.SecureRandom

  val generator = new SecureRandom(seed)

  override def next:BA = {
    var ret = new BA(seed.length)
    generator.nextBytes(ret)
    ret
  }

}

case class TrivialExtend(seed: BA) extends CryptoExtendable(seed) {
  override def next = seed
}