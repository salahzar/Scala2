package com.pakkio.binary

import scala.collection.immutable.IndexedSeq

/**
 * Created by Francesco on 14/08/2015.
 */



object Week2Crypto {
  implicit def convert2R(x:String):R=R(x)
  implicit def int2R(i: Int): R = {
    def nextPow2(i: Int, acc: Int): Int = if (i < acc) acc else nextPow2(i, 2 * acc)
    val s:String=(nextPow2(i, math.pow(2,4).toInt)+i).toBinaryString.substring(1)
    R(s)

  }
  case class R(_4bits:String){
    def ^(that:R):R ={
      val zipped: List[(Char, Char)] = this._4bits.zip(that._4bits).toList
      val list_chars: List[Char] = zipped.map(
        (pair: (Char, Char)) => pair match {
          case ('0', '0') => '0'
          case ('1', '1') => '0'
          case ('0', '1') => '1'
          case ('1', '0') => '1'

        }
      )
      R(list_chars.mkString)
    }

    // extract ith index
    def apply(i:Int):Int = {
      Integer.parseInt(_4bits.charAt(i).toString,2)
    }
    def toInt = {
      Integer.parseInt(_4bits,2)
    }
    override def toString = _4bits
    def ==(that:R): Unit ={
      _4bits.equals(that._4bits)
    }
  }
  case class R5(k:(R,R,R,R,R)) {
    def apply(i:Int):R={
      val value:R = R(k.productElement(i).toString)
      value
    }

    def foreach[U](f: Any => U) = {
      k.productIterator.map(f)
    }
  }

  def F(k:R5,x:R):R = {
    var ret=k(0)
    for (i <- 0 until 4){
      if(x(i) == 1)
        ret = ret ^ k(i+1)
    }
    ret
  }






}
