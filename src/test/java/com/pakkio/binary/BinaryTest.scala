package com.pakkio.binary

import org.scalatest.FunSuite

/**
 * Created by Francesco on 29/08/2015.
 */
class BinaryTest extends FunSuite {


  test("testFillTo") {
    val b0=new Binary(Array[Byte]())
    assert(b0.size===0)
    val b=new Binary(Array[Byte](1,1,1),4)
    assert(b.size===4)
    val b2=new Binary(Array[Byte](5,5,5,5,5),4)
    assert(b2.size===4)

  }
  test("testUnderlying") {
    val b0=new Binary(Array[Byte]())
    assert(b0.underlying===0)
    val b=new Binary(Array[Byte](1,1,1),4)
    assert(b.underlying===256*256+256+1)
    val b2=new Binary(Array[Byte](5,5,5,5,5),4)
    assert(b2.underlying===(5 * Math.pow(256,3) + 5 * Math.pow(256,2) + 5*256 + 5*1))


  }


  test("testSize") {

    val bytes: Array[Byte] = (0 until 50).map(i => i.toByte).toArray
    val b=new Binary(bytes)
    assert(b.size===50)

  }


  test("testToIntArray") {
    val b=new Binary(Array[Byte](1,2,-3),4)
    assert(b.toIntArray === Array[Int](0,1,2,253))
  }

  test("testToByteArray") {
    val b=Binary(Array[Byte](1,2,-3),4)
    assert(b.toByteArray === Array[Byte](0,1,2,-3))
  }
  test("from hex"){
    val b=Binary.fromHex("ffff",4)
    assert(b.toByteArray===Array[Byte](0,0,-1,-1))
  }
  test("to hex"){
    val b=Binary.fromHex("ffff",4).toHex()
    assert(b==="0000ffff")
  }
  test("xor tests"){
    val b=Binary.fromHex("ffff",16)
    val b1=Binary.fromHex("ff",16)
    val out=b.xor(b1)

  }

}
