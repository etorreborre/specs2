package org.specs2
package io

import scala.collection.mutable.ListBuffer

/**
 * Mock implementation of the Output trait to gather messages.
 *
 * !! This implementation is mutable, so it is not thread-safe !!
 */
private[specs2]
trait MockOutput extends Output {
  private val msgs: ListBuffer[String] = new ListBuffer
  /**  @return the list of stored messages */
  def messages = msgs.toList
  /**
   * if a message is printed with a newline it is considered as being a new message
   * otherwise it is added to the last message
   */
  override def printf(s: String, args: Any*): Unit = {
	  val formatted = s format (args : _*)
	  if (formatted.endsWith("\n"))
	    msgs += formatted.dropRight(1)
	  else if (msgs.isEmpty)
	    msgs += formatted
	  else {
	    val last = msgs.last 
	    msgs.dropRight(1)
	    msgs += (last + formatted)
	  }
  }
  
  def clear() = msgs.clear()
}