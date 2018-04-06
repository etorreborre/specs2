package org.specs2
package io

import scala.collection.mutable.ListBuffer

/**
 * Mock implementation of the Output trait to gather messages.
 *
 * !! This implementation is mutable, so it is not thread-safe !!
 */
trait StringOutput extends Output {

  private val msgs: ListBuffer[String] = new ListBuffer

  /**  @return the list of stored messages */
  def messages = msgs.toList
  /** @return the output as one string */
  def output = messages.mkString("\n")

  /**
   * if a message is printed with a newline it is considered as being a new message
   * otherwise it is added to the last message
   */
  override def printf(s: String, args: Any*): Unit = {
    val formatted = s format (args : _*)
    if (formatted.endsWith("\n"))
      append(formatted.dropRight(1))
    else if (msgs.isEmpty)
      append(formatted)
    else {
      val last = msgs.last
      msgs.remove(msgs.size - 1)
      append(last + formatted)
    }
    ()
  }

  protected def append(msg: String) = synchronized { msgs += msg; () }
  
  def clear(): Unit = { msgs.clear() }
}
