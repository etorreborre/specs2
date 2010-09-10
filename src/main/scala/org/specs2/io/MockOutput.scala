package org.specs2
package io
import scala.collection.mutable.ListBuffer

/**
 * Mock implementation of the Output trait to gather messages
 */
trait MockOutput extends Output {
  val messages: ListBuffer[String] = new ListBuffer

  /**
   * if a message is printed with a newline it is considered as being a new message
   * otherwise it is added to the last message
   */
  override def printf(s: String, args: Any*): Unit = {
	val formatted = s format (args : _*)
	if (formatted.endsWith("\n"))
	  messages += formatted
	else if (messages.isEmpty)
	  messages += formatted
	else {
	  val last = messages.last 
	  messages.dropRight(1)
	  messages += (last + formatted)
	}
  }
}