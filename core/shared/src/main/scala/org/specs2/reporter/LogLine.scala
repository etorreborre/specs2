package org.specs2
package reporter

/** ADT for logging strings as info, warning or errors
  */
sealed trait LogLine:
  def log(logger: PrinterLogger): Unit

case class InfoLine(s: String) extends LogLine { def log(logger: PrinterLogger) = logger.infoLog(s) }
case class ErrorLine(s: String) extends LogLine { def log(logger: PrinterLogger) = logger.errorLog(s) }
case class FailureLine(s: String) extends LogLine { def log(logger: PrinterLogger) = logger.failureLog(s) }
case object EmptyLine extends LogLine { def log(logger: PrinterLogger) = logger.newline() }

object LogLine:
  extension (s: String) def info = InfoLine(s)
  def error = ErrorLine(s)
  def failure = FailureLine(s)
