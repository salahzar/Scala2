package com.pakkio.binary

import com.pakkio.binary.Week2Crypto._
import org.scalatest.FunSuite

/**
 * Created by Francesco on 14/08/2015.
 */
class Week2CryptoTest extends FunSuite {


  test("can create some R and they can be tested") {
    val r = R("0111")
    val r1 = R("0111")

    val r2 = R("1111")
    assert(r !== r2)
    assert(r === r1)
    assert(r2(0) === 1)
    assert(r1(0) === 0)
  }
  test("can do correct xor") {
    val r = R("0111")
    val r2 = R("1010")

    val r3 = r ^ r2
    val r3int: Int = r3.toInt
    val rint: Int = r.toInt
    val r2int: Int = r2.toInt
    val xored: Int = rint ^ r2int
    assert(r3int === xored)
  }
  test("R5 is working") {
    val k = R5(R("0000"), R("0001"), R("0010"), R("0011"), R("0100"))
    val k0: R = k(0)
    assert(k(0) === R("0000"))
    assert(k(1) === R("0001"))
    assert(k(2) === R("0010"))
    assert(k(3) === R("0011"))
    assert(k(4) === R("0100"))
  }
  test("can apply F") {
    val k = R5("0000", "0001", "0010", "0011", "0100")

    val x = R("0100") // k0 ^ k2
    val x2 = R("1110") // all xored except k4
    assert(F(k, x) === (k(0) ^ k(2)))
    var allxored = R("0000")

    allxored = k(0) ^ k(1) ^ k(2) ^ k(3)// ^ k(4)

    val value_computed: R = F(k, x2)
    assert(value_computed === allxored)


  }
  test("actual exercise"){
    val v_0110=R("0011")
    val v_0101=R("1010")
    val v_1110=R("0110")
  }

}
