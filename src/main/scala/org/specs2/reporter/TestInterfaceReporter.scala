package org.specs2
package reporter

import _root_.org.scalatools.testing.{ EventHandler, Logger, Event, Result }
import control.Exceptionx._
import io._
import text._
import execute.{ Success, Failure, Error, Skipped, Pending }
import specification._
/**
 * 
 */
class TestInterfaceReporter(val handler: EventHandler, val loggers: Array[Logger]) extends ConsoleReporter 
  with LoggedOutput with HandlerEvents {  
	
  override val executeFragment: Function[Fragment, ExecutedFragment] = (f: Fragment) => {
 	  val executed = new FragmentExecution {}.executeFragment(f)
    executed match {
      case ExecutedResult(text: String, result: org.specs2.execute.Result) => result match {
        case Success(text) => handler.handle(succeeded(text)) 	
        case r @ Failure(text, e) => handler.handle(failure(text, r.exception))
        case r @ Error(text, e) => handler.handle(error(text, r.exception))
        case Skipped(text) => handler.handle(skipped(text))
        case Pending(text) => handler.handle(skipped(text))
      }
      case _ => ()
    }
    executed
  }
}
trait LoggedOutput extends Output with TestLoggers {
  override def printf(format: String, args: Any*) = loggers.foreach { logger =>
    if (logger.ansiCodesSupported)
      logger.info(format.format(args:_*))
    else
      logger.info(AnsiColors.removeColors(format.format(args:_*)))	
  }
}
/**
 * Specific events which can be notified to sbt
 */
trait HandlerEvents {
  class NamedEvent(name: String) extends Event {
    def testName = name
    def description = ""
    def result = Result.Success
    def error: Throwable = null
  }
  def succeeded(name: String) = new NamedEvent(name)
  def failure(name: String, e: Throwable) = new NamedEvent(name) {
    override def result = Result.Failure
    override def error = e
  }
  def error(name: String, e: Throwable) = new NamedEvent(name) {
    override def result = Result.Error
    override def error = e
  }
  def skipped(name: String) = new NamedEvent(name) {
    override def result = Result.Skipped
    override def error = null
  }
}

trait TestLoggers {
  val loggers: Array[Logger]
  def logError(message: String) = loggers.foreach { logger =>
    if (logger.ansiCodesSupported)
      logger.error(AnsiColors.red + message + AnsiColors.reset)
    else
      logger.error(message)
  }
  def logInfo(message: String, color: String) = loggers.foreach { logger =>
    if (logger.ansiCodesSupported)
      logger.info(color + message + AnsiColors.reset)
    else
      logger.info(message)
  }
  def logStatus(name: String, color: String, status: String) = {
    logInfo(status + " " + name, color)
  }
  def logErrorDetails(e: Exception) = {
    logStatus(e.getMessage + " (" + e.location + ")", AnsiColors.red, " ")
  }
}