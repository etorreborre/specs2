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
  private val buffer = new StringBuilder

  private def info(msg: String)(implicit args: Arguments) {
    buffer.append(offset(msg))
  }

  def flushText(force: Boolean = false)(implicit args: Arguments) = {
    if (force || endsWith(buffer.toString, "\n")) {
      infoLog(buffer.toString)
      buffer.clear
    }
  }

  private def endsWith(message: String, string: String)(implicit args: Arguments) = {
    val nocolor = args.colors.removeColors(message)
    message.nonEmpty &&
      message.reverse.
      dropWhile(_ == ' ').
      startsWith(string)
  }

  def printSeeLink(message: String, stats: Stats)(implicit args: Arguments)        = info(status(stats.result)+args.textColor(message))
  def printText(message: String)(implicit args: Arguments)                         = info(args.textColor(message))
  def printSuccess(message: String)(implicit args: Arguments)                      = info(message)
  def printSkipped(message: String)(implicit args: Arguments)                      = info(message)
  def printPending(message: String)(implicit args: Arguments)                      = info(message)
  def printFailure(message: String)(implicit args: Arguments)                      = { flushText(force=true); failureLog(offset(message)) }
  def printError(message: String)(implicit args: Arguments)                        = { flushText(force=true); errorLog(offset(message)) }

  def printSpecStartTitle(message: String, stats: Stats)(implicit args: Arguments) = printSpecStartName(message, stats)
  def printSpecStartName(message: String, stats: Stats)(implicit args: Arguments)  = { flushText(); info(message) }
  def printStats(message: String)(implicit args: Arguments)                        = { info(message); flushText(force=true) }
  def status(result: Result)(implicit args: Arguments): String                     = result.coloredStatus(args) + " "

  def printMessage(message: String)(implicit args: Arguments)                      = printLines(args.textColor(message))
  def printLine(message: String)(implicit args: Arguments)                         = { info(message); flushText(force=true) }

  def printLines(message: String)(implicit args: Arguments)                        = printLine(offset(message))
  /** add an offset to the message */
  private def offset(message: String)(implicit args: Arguments) = message.offset(args.offset)

}

/**
 * Logger with info, failure, error where each new message is displayed on a new line
 */
trait LineLogger {
  def infoLog(msg: String)
  def failureLog(msg: String)
  def errorLog(msg: String)
}


