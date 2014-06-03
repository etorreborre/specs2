package org.specs2
package reporter

import scalaz.stream.Process

sealed trait LogLine {
  def log(logger: LineLogger)
}
case class InfoLine(s: String)    extends LogLine { def log(logger: LineLogger) = logger.infoLog(s) }
case class ErrorLine(s: String)   extends LogLine { def log(logger: LineLogger) = logger.errorLog(s) }
case class FailureLine(s: String) extends LogLine { def log(logger: LineLogger) = logger.failureLog(s) }
case object EmptyLine             extends LogLine { def log(logger: LineLogger) = () }

object LogLine {
  implicit class toInfoLine(s: String)    { def info = InfoLine(s) }
  implicit class toErrorLine(s: String)   { def error = ErrorLine(s) }
  implicit class toFailureLine(s: String) { def failure = FailureLine(s) }

  implicit class processToInfoLine   (p: Process[Nothing, String]) { def info    = p.map(InfoLine.   apply) }
  implicit class processToErrorLine  (p: Process[Nothing, String]) { def error   = p.map(ErrorLine.  apply) }
  implicit class processToFailureLine(p: Process[Nothing, String]) { def failure = p.map(FailureLine.apply) }
}



