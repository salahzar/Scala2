
import java.nio.charset.{StandardCharsets, Charset}

import com.pakkio.binary.{Binary, Buffer}
import com.pakkio.binary.Week2Crypto.R

import org.scalatest.FunSuite
import sun.misc.HexDumpEncoder
import xyz.wiedenhoeft.scalacrypt.blockciphers.AES128
import xyz.wiedenhoeft.scalacrypt._
import xyz.wiedenhoeft.scalacrypt.modes.CBC
import xyz.wiedenhoeft.scalacrypt.paddings.PKCS7Padding

import scala.collection.mutable
import scala.util.{Try, Failure, Success}


class CBCTest extends FunSuite {
  def cryptCipher(key:Key, iv:Seq[Byte] )={
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
  def cryptCBC(key:Key, iv:Array[Byte], msg: String): Try[Array[Byte]] ={

    val ivBlock = new Binary(iv,16)
    cryptCipher(key,ivBlock.toByteArray).encrypt(msg.map(_.toByte)) match {
      case Success(x) => Success(ivBlock.toByteArray ++ x)
      case Failure(e) => {
        print(s"Failing cryptCBC because  error $e")
        Failure(e)
      }
    }
  }
  def decryptCBC(key:Key, cipherText:Array[Byte]): Try[String] ={
    val iv=cipherText.slice(0,16)
    val rest=cipherText.drop(16)
    cryptCipher(key,iv).decrypt(rest) match {
      case Success(s) => Success(s.map(byte => byte.toChar).mkString)
      case Failure(e) => {
        print(s"Failing decryptCBC because error $e")
        Failure(e)
      }
    }

  }

  val key: Key = Key.generate[SymmetricKey128]

  test("testing encode and decode") {
    val original="Initial Value"
    val iv=Array[Byte](1)
    cryptCBC(key,iv,original).map { encrypted =>
      //val hex=BaseN(encrypted).toString
      //println(s"encrypted is ${hex}")
      decryptCBC(key,encrypted).map { decrypted =>
        val ascii = decrypted.mkString
        println(s"decrypted is ${ascii}")
        assert(original == ascii)
      }
    }
  }
  test("try to forge a cbc encryption"){
    val plainText="$100 to Bob"

  }
}