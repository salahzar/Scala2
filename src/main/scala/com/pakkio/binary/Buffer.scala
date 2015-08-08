package com.pakkio.binary

case class Buffer(content:Array[Byte]) {
  def xor(y: Buffer):Buffer = Buffer(content.zip(y.content).map{
    case (x,y) =>
      (x ^ y).toByte
  })


  def toHex =  content.map("%02X".format(_)).mkString("")
}

object Buffer {
  def apply(s: String) = {
    new Buffer(s.map(_.toByte).toArray)
  }
  def apply(l: List[Int]) =
    new Buffer(l.map(_.toByte).toArray)
  def fromHex(s: String) = {
    new Buffer(
      s.sliding(2,2).map(Integer.parseInt(_,16).toByte).toArray
    )
  }
}

trait Implicits {
  implicit def convertStringsToByteArrays(s: String): Array[Byte] =
    s.map(_.toByte).toArray
  implicit def convertSrtringsToBuffer(s:String): Buffer = Buffer(s)

}