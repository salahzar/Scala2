package com.pakkio.binary

import org.scalatest.FunSuite

/**
 * Created by pakki_000 on 08/08/2015.
 */
class Conversions extends FunSuite with Implicits {

  test("testConvertStringsToByteArrays") {
      val s="alle falde"

      val ba:Array[Byte]=s

      assert(ba(0)===97)
      assert(ba.length===s.length)

  }

}
