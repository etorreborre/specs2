package org.specs2
package text

private[specs2]
trait Message {
  /**
   * concatenate 2 messages
   */
  def concat(m1: String, m2: String, separator: String = "; ") =
    if (m1.isEmpty)      m2
    else if (m2.isEmpty) m1
    else                 m1+separator+m2

}

private[specs2]
object Message extends Message