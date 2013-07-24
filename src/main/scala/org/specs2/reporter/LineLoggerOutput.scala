package org.specs2
package reporter

import main.Arguments
import specification.Stats
import text.Trim._
import org.specs2.execute.Result

/**
 * This outputs the textual results of an executed specification to a LineLogger
 */
trait LineLoggerOutput extends ResultOutput with LineLogger {
  private val output = new TextResultOutput
  private val buffer = new StringBuilder

  private def info(msg: String)(implicit args: Arguments) {
    buffer.append(output.offset(msg))
  }

  def flushText(force: Boolean = false) = {
    if (force ||
        buffer.nonEmpty && endsWith(buffer.toString, "\n")) {
      infoLog(buffer.toString)
      buffer.clear
    }
  }

  private def endsWith(message: String, string: String) = {
    message.reverse.
      dropWhile(_ == ' ').
      startsWith(string)
  }

  def printSeeLink(message: String, stats: Stats)(implicit args: Arguments)        = info(status(stats.result)+args.textColor(message))
  def printText(message: String)(implicit args: Arguments)                         = info(message)
  def printLine(message: String)(implicit args: Arguments)                         = { info(message); flushText(force=true) }
  def printSuccess(message: String)(implicit args: Arguments)                      = info(message)
  def printSpecStartName(message: String, stats: Stats)(implicit args: Arguments)  = { flushText(); info(message) }
  def printSpecStartTitle(message: String, stats: Stats)(implicit args: Arguments) = printSpecStartName(message, stats)
  def printSkipped(message: String)(implicit args: Arguments)                      = { flushText(force=true); info(message) }
  def printPending(message: String)(implicit args: Arguments)                      = { flushText(force=true); info(message) }
  def printFailure(message: String)(implicit args: Arguments)                      = { flushText(force=true); failureLog(output.offset(message)) }
  def printError(message: String)(implicit args: Arguments)                        = { flushText(force=true); errorLog(output.offset(message)) }
  def printStats(message: String)(implicit args: Arguments)                        = { info(message); flushText(force=true) }
  def printMessage(message: String)(implicit args: Arguments)                      = printLines(args.textColor(message))
  def printLines(message: String)(implicit args: Arguments)                        = printLine(output.offset(message))
  def status(result: Result)(implicit args: Arguments): String                     = output.status(result)
}

/**
 * Logger with info, failure, error where each new message is displayed on a new line
 */
trait LineLogger {
  def infoLog(msg: String)
  def failureLog(msg: String)
  def errorLog(msg: String)
}


