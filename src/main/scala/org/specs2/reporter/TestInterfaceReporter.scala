package org.specs2
package reporter

import _root_.org.scalatools.testing.{ EventHandler, Logger, Event, Result }
import main.Arguments
import text._
import Trim._
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

  override def export(implicit args: Arguments): ExecutingSpecification => ExecutedSpecification = (spec: ExecutingSpecification) => {
    super.export(args)(spec)
    val executed = spec.execute
    executed.fragments foreach handleFragment(args)
    executed
  }

  protected def handleFragment(implicit args: Arguments): ExecutedFragment => ExecutedFragment = (f: ExecutedFragment) => {
    f match {
      case ExecutedResult(text: FormattedString, result: org.specs2.execute.Result, timer: SimpleTimer, _, _) => {
        def handleResult(res: org.specs2.execute.Result) {
          res match {
            case Success(_,_)             => handler.handle(succeeded(text.raw))
            case r @ Failure(_, e, st, d) => handler.handle(failure(text.raw, args.traceFilter(r.exception)))
            case r @ Error(_, e)          => handler.handle(error(text.raw, args.traceFilter(r.exception)))
            case Skipped(d, _)            => handler.handle(skipped(text.raw, d))
            case Pending(d)               => handler.handle(skipped(text.raw, d))
            case DecoratedResult(t, r)    => handleResult(r)
          }
        }
        handleResult(result)
        f
      }
      case _                                 => f
    }
  }
}

class TestInterfaceResultOutput(val loggers: Array[Logger]) extends LineLoggerOutput with TestLoggers {
  def infoLog(msg: String)    = logInfo(msg)
  def failureLog(msg: String) = logFailure(msg)
  def errorLog(msg: String)   = logError(msg)
}

/**
 * Specific events which can be notified to sbt
 */
trait HandlerEvents {
  class NamedEvent(name: String, desc: String = "") extends Event {
    def testName = name
    def description = if (desc.isEmpty) name else desc
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
  def skipped(name: String, description: String = "") = new NamedEvent(name, description) {
    override def result = Result.Skipped
    override def error = null
  }
  def result(name: String)(r: execute.Result): NamedEvent = r match {
    case s @ execute.Success(_, _)             => succeeded(name)
    case f @ execute.Failure(_,_,_,_)          => failure(name, f.exception)
    case e @ execute.Error(_,_)                => error(name, e.exception)
    case p @ execute.Pending(d)                => skipped(name, d)
    case k @ execute.Skipped(d,_)              => skipped(name, d)
    case d @ execute.DecoratedResult(dec, res) => result(name)(res)
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