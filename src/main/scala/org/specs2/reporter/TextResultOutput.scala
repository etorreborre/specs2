package org.specs2
package reporter

import io.ConsoleOutput
import main.Arguments
import text.Plural._
import text.AnsiColors._
import execute.Result
/**
 * Implementation of the ResultOutput trait as Text
 */
class TextResultOutput extends ResultOutput with ConsoleOutput {

  def printSpecStart(message: String)(implicit args: Arguments) = {
    printLines(message)
  }
  def printSuccess(message: String)(implicit args: Arguments) = {
    printLines(message)
  }
  def printFailure(message: String)(implicit args: Arguments) = {
    printLines(color(message, yellow, args.color))
  }
  def printError(message: String)(implicit args: Arguments) = {
    printLines(color(message, red, args.color))
  }
  def printSkipped(message: String)(implicit args: Arguments) = {
    printLines(message)
  }
  def printPending(message: String)(implicit args: Arguments) = {
    printLines(message)
  }
  def status(result: Result)(implicit args: Arguments): String = {
    if (args.plan) ""
    else (result.status(args)  + " ")
  }

  
  /**
   * print some text, splitting it on several lines
   */
  def printMessage(message: String)(implicit args: Arguments) = {
    printLines(color(message, white, args.color))
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
