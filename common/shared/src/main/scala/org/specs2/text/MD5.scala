package org.specs2
package text

import java.security.MessageDigest
import scala.collection.immutable.ArraySeq

/**
 * MD5 functions
 */
private[specs2]
trait MD5 {

  /** @return the MD5 hash of a sequence of bytes as a String */
  def md5Hex(bytes: Array[Byte]): String = md5Bytes(bytes).map("%02X".format(_)).mkString.toLowerCase

  /** @return the MD5 hash of a sequence of bytes as bytes */
  def md5Bytes(bytes: Array[Byte]): Seq[Byte] =
    ArraySeq.unsafeWrapArray(MessageDigest.getInstance("MD5").digest(bytes))

}

private[specs2]
object MD5 extends MD5
