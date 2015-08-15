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
  test("actual exercise with xor"){
    val v_0110=R("0011") // k0+k2+k3
    val v_0101=R("1010") // k0+k2+k4
    val v_1110=R("0110") // k0+k1+k2+k3
    val k1=v_0110 ^
           v_1110

    val k1k3 = v_0101 ^
               v_1110

    val k3k4 = v_0110 ^
               v_0101

    val k3 = k1 ^ k1k3

    val k4 = k3 ^ k3k4

    val v_1101= v_1110 ^ k3 ^ k4

    println(s"Value for 1101 is $v_1101")



  }

  /*def evaluateDataForThisKey(k: R5) = {
    if(
      (F(k,"0110")=="0011") &&


  }

  import scala.math.pow

  test("Brutal force"){
    val exponent = pow(16,5)
    println(s"should loop over ${exponent} values")
    for(
      k0 <- 0 to 15;
      k1 <- 0 to 15;
      k2 <- 0 to 15;
      k3 <- 0 to 15;
      k4 <- 0 to 15){
      val k=R5(k0,k1,k2,k3,k4)

      evaluateDataForThisKey(k)
    }

  }
*/
}
