package com.pakkio.binary

import org.scalatest.FunSuite
import Implicits._

class Conversions extends FunSuite {

  test("testConvertStringsToByteArrays") {
    val s = "alle falde"

    val ba: Array[Byte] = s

    assert(ba(0) === 97)
    assert(ba.length === s.length)

  }

}
