package org.specs2
package reporter

import scalaz.stream.Process

/**
 * ADT for logging strings as info, warning or errors
 */
sealed trait LogLine {
  def log(logger: LineLogger)
}

case class InfoLine(s: String)    extends LogLine { def log(logger: LineLogger) = logger.infoLog(s) }
case class ErrorLine(s: String)   extends LogLine { def log(logger: LineLogger) = logger.errorLog(s) }
case class FailureLine(s: String) extends LogLine { def log(logger: LineLogger) = logger.failureLog(s) }
case object EmptyLine             extends LogLine { def log(logger: LineLogger) = logger.newline }

object LogLine {
  implicit class toInfoLine(s: String)    { def info = InfoLine(s) }
  implicit class toErrorLine(s: String)   { def error = ErrorLine(s) }
  implicit class toFailureLine(s: String) { def failure = FailureLine(s) }
}



