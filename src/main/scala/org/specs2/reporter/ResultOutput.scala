package org.specs2
package reporter

import io._
import text.Plural._
import text.AnsiColors._
import main.Arguments
import specification._

/**
 * This traits provides specialised print methods for message representing
 * different types of results. They can be overridden to define a more specific behaviour.
 */
trait ResultOutput extends Output {

  def printSuccess(message: String)(implicit args: Arguments) = {
    printLines(color(message, green, args.color))
  }
  def printError(message: String)(implicit args: Arguments) = {
    printLines(color(message, red, args.color))
  }
  def printSkipped(message: String)(implicit args: Arguments) = {
    printLines(color(message, white, args.color))
  }
  def printPending(message: String)(implicit args: Arguments) = {
    printLines(color(message, white, args.color))
  }
  
  /**
   * print some text, splitting it on several lines
   */
  def printMessage(message: String)(implicit args: Arguments) = {
    printLines(color(message, blue, args.color))
  }
  
  def printLines(message: String)(implicit args: Arguments) = {
    val splitted = message.split("\n")
    if (splitted.size > 1) splitted.foreach(m => printLine(m))
    else printLine(message)
  }
  /**
   * print one line
   */
  def printLine(message: String)(implicit args: Arguments) = {
    println((" "*args.offset) + message)
  }
}
