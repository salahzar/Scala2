
import java.nio.charset.{StandardCharsets, Charset}

import com.pakkio.binary.Buffer
import com.pakkio.binary.Week2Crypto.R
import org.scalatest.FunSuite
import sun.misc.HexDumpEncoder
import xyz.wiedenhoeft.scalacrypt.blockciphers.AES128
import xyz.wiedenhoeft.scalacrypt._
import xyz.wiedenhoeft.scalacrypt.modes.CBC
import xyz.wiedenhoeft.scalacrypt.paddings.PKCS7Padding


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

  implicit def string2byte(s:String):List[Byte]=s.getBytes(StandardCharsets.US_ASCII).toList
  implicit def seq2list(s:Seq[Byte]):List[Byte]=s.toList

  val key: Key = Key.generate

  test("testing encode and decode") {

    val suite=cryptCipher(key,iv)
    val original="Initial Value"
    val encrypted = suite.encrypt(original).get

    val buffer = Buffer.fromBytes(params('iv))Buffer.fromBytes(encrypted)
    println(s"encrypted is ${buffer.toHex}")


    val decrypted = suite.decrypt(encrypted).get

    val returned = decrypted.map(_.toChar).mkString
    assert(original==returned)

  }
}