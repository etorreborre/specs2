package org.specs2
package reporter

import io.ConsoleOutput
import main.Arguments
import text.Plural._
import text.AnsiColors._

/**
 * Implementation of the ResultOutput trait as Text (to a console possibly with AnsiColors)
 */
class TextResultOutput extends ResultOutput with ConsoleOutput {

  def printSpecStart(message: String)(implicit args: Arguments) = {
    printLines(message)
  }
  def printSuccess(message: String)(implicit args: Arguments) = {
    printLines(color(message, green, args.color))
  }
  def printFailure(message: String)(implicit args: Arguments) = {
    printLines(color(message, yellow, args.color))
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
