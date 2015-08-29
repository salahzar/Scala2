

import javax.crypto.BadPaddingException

import com.pakkio.binary.{Binary}

import org.scalatest.FunSuite
import xyz.wiedenhoeft.scalacrypt
import xyz.wiedenhoeft.scalacrypt.blockciphers.AES128
import xyz.wiedenhoeft.scalacrypt._
import xyz.wiedenhoeft.scalacrypt.modes.CBC
import xyz.wiedenhoeft.scalacrypt.paddings.PKCS7Padding

import scala.util.{Try, Failure, Success}


class CBCTest extends FunSuite {
  def cryptCipher(key: Key, iv: Seq[Byte]) = {
    val params = Parameters(
      'symmetricKey128 -> key,
      'iv -> iv
    )

    // When you dynamically build the params object you might want to match the
    // Try objects for error handling
    val aes = BlockCipher[AES128](params).get
    val cbc = BlockCipherMode[CBC](params).get
    val pkcs7 = BlockPadding[PKCS7Padding](params).get

    new BlockCipherSuite(aes, cbc, pkcs7)

  }

  def cryptCBC(key: Key, iv: Array[Byte], msg: String): Try[Array[Byte]] = {

    val ivBlock = new Binary(iv, 16)
    cryptCipher(key, ivBlock.toByteArray).encrypt(msg.map(_.toByte)) match {
      case Success(x) => Success(ivBlock.toByteArray ++ x)
      case Failure(e) => {
        // println(s"Failing cryptCBC because  error $e")
        Failure(e)
      }
    }
  }

  def decryptCBC(key: Key, cipherText: Array[Byte]): Try[String] = {
    val iv = cipherText.slice(0, 16)
    val rest = cipherText.drop(16)
    cryptCipher(key, iv).decrypt(rest) match {
      case Success(s) => Success(s.map(byte => byte.toChar).mkString)
      case Failure(e) => {
        // println(s"Failing decryptCBC because error $e")
        Failure(e)
      }
    }

  }

  val key: Key = Key.generate[SymmetricKey128]
  val iv = generateIV

  def generateIV = {
    import scala.util.Random
    val iv = Array.fill[Byte](16)(0)
    new Random(0).nextBytes(iv)
    iv
  }

  test("testing encode and decode") {
    val original = "Initial Value"

    cryptCBC(key, iv, original).map { encrypted =>
      val hex = Binary(encrypted).toHex
      println(s"encrypted is ${hex}")
      decryptCBC(key, encrypted).map { decrypted =>
        val ascii = decrypted.mkString
        println(s"decrypted is ${ascii}")
        assert(original == ascii)
      }
    }
  }
  test("try to forge a cbc encryption") {
    val plainText = "$100 to Bob"
    def forge2ndCharacterInIV(encrypted: Array[Byte]): Binary = {
      val forged = Binary(encrypted).xor(1, '1' ^ '5')
      forged
    }
    cryptCBC(key, iv, plainText).map {
      encrypted =>

        val forged: Binary = forge2ndCharacterInIV(encrypted)
        decryptCBC(key, forged.toByteArray).map {
          decrypted =>
            println(decrypted)
            assert(decrypted === "$500 to Bob")

        }
    }


  }
  test("try to create a padding error") {
    val plainText = "Hello"
    cryptCBC(key, iv, plainText).map {
      encrypted =>

        for(pos<- (0 until 16).reverse) {
          val revealed: Int = revealByteAtPos(encrypted, pos)
          println("Revealed byte is 0x%02x".format(revealed))
        }



    }
  }


  def revealByteAtPos(encrypted: Array[Byte], pos: Int): Int = {


    val guessed = guessPadding(encrypted, pos)
    println("Good padding with byte 0x%02x".format(guessed))
    // now try to understand which value was in intermediate place
    // since we know this decodes to 01

    val iByte = (guessed ^ 1)

    // but now we knew the value of ivbytes from first block of encrypted 15th position

    val ivByte = encrypted(pos)

    // now decrypted text in that position is simply the xor of iByte and ivByte
    val revealed = (iByte ^ ivByte)
    revealed
  }

  def guessPadding(encrypted: Array[Byte], pos: Int): Byte = {
    for (g <- 0 until 256) {
      // this is a surely invalid IV for that
      val iv1 = Array.fill[Byte](16)(0)
      val padlength = 16 - pos
      // fill latest characters with the same number padLength
      for(i<- pos until 16){
        iv1(i)=g.toByte
      }

      //iv1(15) = g.toByte
      val forged = iv1 ++ encrypted.drop(16)
      if (paddingOracle(forged)) return g.toByte

    }
    throw new Exception("Can't find a valid guess")
  }

  def paddingOracle(forged: Array[Byte]): Boolean = {
    decryptCBC(key, forged) match {
      case Success(decrypted) =>
        //println(decrypted)
        true
      case Failure(x: scalacrypt.BadPaddingException) =>
        //println("bad padding")
        false
      case Failure(e) =>
        println("generic failure")
        false

    }
  }
}