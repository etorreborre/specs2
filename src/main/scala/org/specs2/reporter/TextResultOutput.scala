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
  def printFailure(message: String)(implicit args: Arguments)                      = {
    if (args.report.flow) printLines("\n"+message)
    else                  printLines(args.failureColor(message))
  }
  def printError(message: String)(implicit args: Arguments)                        = {
    if (args.report.flow) printLines("\n"+message)
    else                  printLines(args.errorColor(message))
  }
  def printSkipped(message: String)(implicit args: Arguments)                      = {
    if (args.report.flow) printLines("\n"+message)
    else                  printLines(args.skippedColor(message))
  }
  def printPending(message: String)(implicit args: Arguments)                      = {
    if (args.report.flow) printLines("\n"+message)
    else                  printLines(args.pendingColor(message))
  }
  def printText(message: String)(implicit args: Arguments)                         = printLines(args.textColor(message))
  def printStats(message: String)(implicit args: Arguments)                        = {
    if (args.report.flow) println("\n\n"+args.statsColor(message))
    else                  printLines(message)
  }
  def status(result: Result)(implicit args: Arguments): String                     = result.coloredStatus(args) + " "
  
  /**
   * print some text, splitting it on several lines
   */
  def printMessage(message: String)(implicit args: Arguments) = printLines(args.textColor(message))
  
  def printLines(message: String)(implicit args: Arguments) = {
    if (args.report.flow) {
      print(message.replace("\n", "\n"+(" "*args.offset)))
    } else {
      val splitted = message.split("\n")
      if (splitted.size > 1) splitted.foreach(m => printLine(m))
      else printLine(message)
    }
  }
  /**
   * print one line
   */
  def printLine(message: String)(implicit args: Arguments) =
    if (args.report.flow && (message.contains('\n') || message.matches("( )+"))) print((" "*args.offset) + message)
    else                                                                        println((" "*args.offset) + message)
  
}

