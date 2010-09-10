package org.specs2
package io
import scala.collection.mutable.ListBuffer

trait MockOutput extends Output {
  val messages: ListBuffer[String] = new ListBuffer
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