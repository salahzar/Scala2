package com.pakkio.binary

import org.scalatest.FunSuite


class BufferTest extends FunSuite  {
    test("can create a Buffer") {
      val x=Buffer("this is a string")

    }
    test("can print a buffer in hexdecimal"){
      val x=Buffer("Aa Bb")
      val s=x.toHex
      assert(s === "4161204262")
    }
    test("can do an exor with pure binary"){
      val x=Buffer(List(1,2,3,255))
      val y=Buffer(List(1,2,4,3))
      val z=x.xor(y)
      assert(z.toHex === "000007FC")
    }

    test("can accept an hex string"){
      val originalHex = "a14243"
      val x=Buffer.fromHex(originalHex)
      assert(x.toHex.toUpperCase() ===  originalHex.toUpperCase)
    }

}
