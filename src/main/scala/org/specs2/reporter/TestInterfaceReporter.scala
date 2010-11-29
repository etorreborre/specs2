package org.specs2
package reporter

import _root_.org.scalatools.testing.{ EventHandler, Logger, Event, Result }
import control.Throwablex._
import control.Throwablex._
import main.Arguments
import io._
import text._
import AnsiColors._
import execute.{ Success, Failure, Error, Skipped, Pending }
import specification._
/**
 * Reporter for the test interface defined for sbt
 * 
 * It prints out the result to the output defined by the sbt loggers
 * and publishes events to sbt event handlers
 */
class TestInterfaceReporter(val handler: EventHandler, val loggers: Array[Logger]) extends 
       ConsoleReporter 
  with HandlerEvents {  
	
  override def print(klass: Class[_], fs: Seq[ExecutedFragment])(implicit arguments: Arguments) = 
    printLines(fs).print(new TestInterfaceResultOutput(loggers))

  override def executeFragment(implicit arguments: Arguments): Function[Fragment, ExecutedFragment] = (f: Fragment) => {
 	  val executed = new FragmentExecution {}.executeFragment(arguments)(f)
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
class TestInterfaceResultOutput(val loggers: Array[Logger]) extends TextResultOutput with TestLoggers {
  override def printFailure(message: String)(implicit args: Arguments) = logFailure(message)
  override def printError(message: String)(implicit args: Arguments) = logError(message)
  override def printSuccess(message: String)(implicit args: Arguments) = logInfo(message, AnsiColors.green)
  override def printLine(message: String)(implicit args: Arguments) = logInfo(message)
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
  def logFailure(message: String, c: String = AnsiColors.yellow) = loggers.foreach { logger =>
    logger.error(color(message, c, logger.ansiCodesSupported))
  }
  def logError(message: String, c: String = AnsiColors.red) = loggers.foreach { logger =>
    logger.error(color(message, c, logger.ansiCodesSupported))
  }
  def logInfo(message: String, c: String = AnsiColors.white) = loggers.foreach { logger =>
    logger.info(color(message, c, logger.ansiCodesSupported))
  }
  def logInfoStatus(name: String, color: String, status: String) = {
    logError(status + " " + name, color)
  }
  def logErrorStatus(name: String, color: String, status: String) = {
    logError(status + " " + name, color)
  }
  def logErrorDetails(e: Throwable): Unit = {
    logErrorStatus(e.getMessage + " (" + e.location + ")", AnsiColors.red, " ")
    e.getStackTrace.foreach { trace =>
      logErrorStatus(trace.toString, AnsiColors.red, " ")
    }
    e.chainedExceptions.foreach(logErrorDetails(_))
  }
}