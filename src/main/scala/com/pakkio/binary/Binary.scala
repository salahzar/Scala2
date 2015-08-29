
package com.pakkio.binary

import java.math.BigInteger

import scala.language.implicitConversions

class Binary(val origBa: Array[Byte],val fillTo:Int = -1)  {


  def toInt(b:Byte):Int = (b + 256) % 256


  def add(that: Int) = {
    val binaryToAdd=Binary(BigInteger.valueOf(that).toByteArray,size)
    // now must loop over LSB adding together from right
    val tuples = ba.zip(binaryToAdd.ba)

    case class Acc(carry: Int, result: List[Byte])
    val result = tuples.foldRight(Acc(0, List[Byte]()))((pair, acc) => {
      val tot = toInt(pair._1) + toInt(pair._2) + acc.carry
      val out = tot % 256
      Acc(tot / 256, out.toByte :: acc.result)
    }
    ).result.toArray
    Binary(result,size)
  }


  def fxor(x:(Byte,Byte)): Byte = {
    val ret=(x._1 ^ x._2).toByte
    println(s"1st: ${x._1}, 2nd: ${x._2} xor: $ret")
    ret
  }

  def xor(that: Binary) = {
    val tuples: Array[(Byte, Byte)] = ba.zip(that.ba)
    val baOut:Array[Byte]=tuples.map(x=>fxor(x))
    Binary(baOut,tuples.length)
  }
  def xor(position:Int, value:Int): Binary ={
    Binary(ba.zipWithIndex.map( (pair) => if(pair._2 == position ) (pair._1 ^ value).toByte else pair._1))
  }
  def set(position:Int, value:Int): Binary ={
    Binary(ba.zipWithIndex.map( (pair) => if(pair._2 == position ) (value).toByte else pair._1))
  }

  def toHex = ba.map("%02x".format(_)).mkString


  // find real byte array filled at head with proper
  val ba=if(fillTo == -1) origBa else {
    if (origBa.length < fillTo) {
      val filler = Array.fill[Byte](fillTo-origBa.length)(0)
      filler ++ origBa
    }
    else origBa.slice(0,fillTo)
  }
  val size=ba.length
  val underlying =
    if(size==0) BigInt(0)
    else BigInt(new BigInteger(ba))


  /**
   * Return this BaseN as an array of Int normalized  in the range 0 -255
   */
  def toIntArray: Array[Int] = ba.map(_ + 256).map(_ % 256)

  /**
   * Return this BaseN as a Byte Array with the sign byte removed (if it exists)
   */
  def toByteArray = ba

  def ^(that: Binary) = xor(that)


}

object Binary {
  def fromHex(hexString: String,size:Int = -1):Binary = {
    apply(hexString.
      replaceAll("[^0-9A-Fa-f]", "").
      sliding(2, 2).
      toArray.
      map(Integer.parseInt(_, 16)
      .toByte),size)

  }

  def apply(ba:Array[Byte],nbytes:Int = -1) =
    new Binary(ba,nbytes)
}

