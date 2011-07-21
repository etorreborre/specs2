package org.specs2
package reporter

import io.ConsoleOutput
import main.Arguments
import text.Plural._
import text.AnsiColors._
import execute.Result
import specification.Stats

/**
 * Implementation of the ResultOutput trait as Text
 */
class TextResultOutput extends ResultOutput with ConsoleOutput {

  def printSpecStart(message: String, stats: Stats)(implicit args: Arguments) = printLines(args.textColor(message))
  def printSuccess(message: String)(implicit args: Arguments)                 = printLines(args.successColor(message))
  def printFailure(message: String)(implicit args: Arguments)                 = printLines(args.failureColor(message))
  def printError(message: String)(implicit args: Arguments)                   = printLines(args.errorColor(message))
  def printSkipped(message: String)(implicit args: Arguments)                 = printLines(args.skippedColor(message))
  def printPending(message: String)(implicit args: Arguments)                 = printLines(args.pendingColor(message))
  def printStats(message: String)(implicit args: Arguments)                   = printLines(args.statsColor(message))
  def status(result: Result)(implicit args: Arguments): String                = result.status(args) + " "
  
  /**
   * print some text, splitting it on several lines
   */
  def printMessage(message: String)(implicit args: Arguments) = printLines(args.textColor(message))
  
  def printLines(message: String)(implicit args: Arguments) = {
    val splitted = message.split("\n")
    if (splitted.size > 1) splitted.foreach(m => printLine(m))
    else printLine(message)
  }
  /**
   * print one line
   */
  def printLine(message: String)(implicit args: Arguments) = 
    println((" "*args.offset) + message)
  
}
