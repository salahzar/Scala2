package com.pakkio.binary
import Implicits._
case class Buffer(content:BA) {

  def xor(y: Buffer):Buffer = Buffer(content.zip(y.content).map{
    case (x,y) =>
      (x ^ y).toByte
  })

  def crypt(key: BA) = {
    val extended=CryptoExtend(key).extend(content.length)
    xor(extended)
  }

  /**
   * Analyze randomicity which is simply the average of repetitions, which for 256 numbers
   * should be 1. This result should be the least possible to indicate we don't have
   * frequencies stacking somewhere
   * @return an index of the randomness.. close to 1 is better
   */
  def analyze() = {
    val count = content.length
    val ret = content.groupBy(_.toByte)
    val mapped = ret.map(p=>p._2.length)
    val avg=(0.0 /: mapped){ _+_ } / mapped.size
    avg / ((count / 256) + 1).toInt
  }

  def toHex =  content.map("%02X".format(_)).mkString("")


  def toStringFull(header:String) = s"\n>>>>>$header is '${toString}', hex is '$toHex', \nrandomness is ${analyze()}"



  override def toString = content.map(printable).mkString
  def findSpaces() = content.map(isspace).mkString
}

object Buffer {


  def apply(s: String) = {
    new Buffer(s.map(_.toByte).toArray)
  }
  def apply(l: List[Int]) =
  new Buffer(l.map(_.toByte).toArray)

  def fromOB(s: List[Option[Byte]]) = {
    s.map {
      case None => '.'
      case Some(b) => b.toChar
    }.mkString
  }


  def fromHex(s: String) = {
    new Buffer(
    s.sliding(2,2).map(Integer.parseInt(_,16).toByte).toArray
    )
  }
}

object Implicits {
  type BA = Array[Byte]
  def ifspace(byte:Byte) = isspace(byte)=='_'
  def isspace(byte: Byte): Char = {
    val Pattern = "([a-zA-Z])".r
    byte.toChar match {
      case Pattern(c) => '_'
      case '\0' => '0'
      case _ => 'X'
    }
  }

  def ismatching(pair:(Char, Char)) : Char = {
    pair match {
      case ('_','_') => '_'
      case _ => 'X'
    }
  }
  def matching(xs:List[String]):String = {
    xs.map(println)
    xs.foldLeft("_"*500)(_.zip(_).map(ismatching).mkString)
  }

  def printable(byte: Byte): Char = {
    val ch=byte.toChar
    if(ch<' ' || ch>'z') '.' else ch
  }

  implicit def convertStringsToByteArrays(s: String): BA =
    s.map(_.toByte).toArray
  implicit def convertStringsToBuffer(s:String): Buffer = Buffer(s)
}