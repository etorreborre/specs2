package org.specs2
package reporter

import _root_.org.scalatools.testing.{ EventHandler, Logger, Event, Result }
import main.Arguments
import text._
import time._
import AnsiColors._
import execute.{ Success, Failure, Error, Skipped, Pending, DecoratedResult }
import specification._

/**
 * Reporter for the test interface defined for sbt
 * 
 * It prints out the result to the output defined by the sbt loggers
 * and publishes events to sbt event handlers
 */
class TestInterfaceReporter(val handler: EventHandler, val loggers: Array[Logger]) extends ConsoleReporter
  with HandlerEvents {

  override def textOutput = new TestInterfaceResultOutput(loggers)

  override def export(implicit args: Arguments): ExecutedSpecification => ExportType = (spec: ExecutedSpecification) => {
    super.export(args)(spec)
    spec.fragments foreach handleFragment(args)
  }

  protected def handleFragment(implicit args: Arguments): ExecutedFragment => ExecutedFragment = (f: ExecutedFragment) => {
    f match {
      case ExecutedResult(text: MarkupString, result: org.specs2.execute.Result, timer: SimpleTimer, _, _) => {
        def handleResult(res: org.specs2.execute.Result) {
          res match {
            case Success(text)               => handler.handle(succeeded(text))
            case r @ Failure(text, e, st, d) => handler.handle(failure(text, args.traceFilter(r.exception)))
            case r @ Error(text, e)          => handler.handle(error(text, args.traceFilter(r.exception)))
            case Skipped(text, _)            => handler.handle(skipped(text))
            case Pending(text)               => handler.handle(skipped(text))
            case DecoratedResult(t, r)       => handleResult(r)
          }
        }
        handleResult(result)
        f
      }
      case p @ PromisedExecutedFragment(_)   => handleFragment(args)(p.get)
      case _                                 => f
    }
  }
}

class TestInterfaceResultOutput(val loggers: Array[Logger]) extends TextResultOutput with TestLoggers {
  override def printSpecStartName(message: String, stats: Stats)(implicit args: Arguments)  = logInfo(message)
  override def printSpecStartTitle(message: String, stats: Stats)(implicit args: Arguments) = logInfo(message)
  override def printFailure(message: String)(implicit args: Arguments)                      = logFailure(message)
  override def printError(message: String)(implicit args: Arguments)                        = logError(message)
  override def printSuccess(message: String)(implicit args: Arguments)                      = logInfo(message)
  override def printStats(message: String)(implicit args: Arguments)                        = logInfo(message)
  override def printLine(message: String)(implicit args: Arguments)                         = logInfo(message)
  override def printText(message: String)(implicit args: Arguments)                         = logInfo(message)
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
object HandlerEvents extends HandlerEvents

trait TestLoggers {
  val loggers: Array[Logger]
  def logFailure(message: String) = loggers.foreach { logger =>
    logger.error(removeColors(message, !logger.ansiCodesSupported))
  }
  def logError(message: String) = loggers.foreach { logger =>
    logger.error(removeColors(message, !logger.ansiCodesSupported))
  }
  def logInfo(message: String) = loggers.foreach { logger =>
    logger.info(removeColors(message, !logger.ansiCodesSupported))
  }
}