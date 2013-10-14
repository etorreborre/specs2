package org.specs2
package text

private[specs2]
trait Message {
  /**
   * concatenate 2 messages
   */
  def concat(m1: String, m2: String, separator: String = "; ") =
    Seq(m1, m2).filterNot(_.isEmpty).mkString(separator)

}

private[specs2]
object Message extends Message