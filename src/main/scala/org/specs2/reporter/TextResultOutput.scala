package org.specs2
package reporter

import io.ConsoleOutput
import main.Arguments
import execute.Result
import specification.Stats

/**
 * Implementation of the ResultOutput trait as Text
 */
class TextResultOutput extends ResultOutput with ConsoleOutput {

  def printSpecStartName(message: String, stats: Stats)(implicit args: Arguments)  = printLines(args.textColor(message))
  def printSpecStartTitle(message: String, stats: Stats)(implicit args: Arguments) = printLines(args.textColor(message))
  def printSeeLink       (message: String, stats: Stats)(implicit args: Arguments) = printLines(status(stats.result)+args.textColor(message))
  def printSuccess(message: String)(implicit args: Arguments)                      = printLines(args.successColor(message))
  def printFailure(message: String)(implicit args: Arguments)                      = printLines(args.failureColor(message))
  def printError(message: String)(implicit args: Arguments)                        = printLines(args.errorColor(message))
  def printSkipped(message: String)(implicit args: Arguments)                      = printLines(args.skippedColor(message))
  def printPending(message: String)(implicit args: Arguments)                      = printLines(args.pendingColor(message))
  def printText(message: String)(implicit args: Arguments)                         = printLines(args.textColor(message))
  def printStats(message: String)(implicit args: Arguments)                        = printLines(message)
  def status(result: Result)(implicit args: Arguments): String                     = result.coloredStatus(args) + " "
  
  /**
   * print some text, splitting it on several lines
   */
  def printMessage(message: String)(implicit args: Arguments) = printLines(args.textColor(message))
  
  def printLines(message: String)(implicit args: Arguments) = printLine(offset(message))

  /**
   * print one line
   */
  def printLine(message: String)(implicit args: Arguments) = offset(message).split("\n").map(println)

  /** add an offset to the message */
  protected def offset(message: String)(implicit args: Arguments) =
    message.split("\n").map((" "*args.offset)+_).mkString("\n")

}

