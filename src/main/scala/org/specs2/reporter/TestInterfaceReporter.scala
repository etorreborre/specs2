package org.specs2
package reporter

import _root_.org.scalatools.testing.{ EventHandler, Logger, Event, Result }
import control.Throwablex._
import main.Arguments
import main.ArgumentsArgs._
import io._
import text._
import time._
import AnsiColors._
import execute.{ Success, Failure, Error, Skipped, Pending, DecoratedResult }
import specification._
import specification.ExecutedFragment._

/**
 * Reporter for the test interface defined for sbt
 * 
 * It prints out the result to the output defined by the sbt loggers
 * and publishes events to sbt event handlers
 */
class TestInterfaceReporter(val handler: EventHandler, val loggers: Array[Logger]) extends ConsoleReporter
  with HandlerEvents {

  /**
   * print the executed fragments.
   *
   * print only the statistics if 'streaming" = true
   */
  override def print(name: SpecName, fs: Seq[ExecutedFragment])(implicit arguments: Arguments) = {
    if (arguments.report.streaming)
      printLines(fs).print(new TestInterfaceStatsOnlyResultOutput(loggers))
    else {
      printLines(fs).print(new TestInterfaceResultOutput(loggers))
      //fs foreach handleFragment(arguments)
    }
  }

  override def export(implicit args: Arguments): ExecutedSpecification => ExportType = (spec: ExecutedSpecification) => {
    print(spec.name, spec.fragments)
  }

  /**
   * if "streaming" is true, execute a Fragment and print it right away (during the execution phase of the reporter)
   */
  /**
   * if we want to stream the results, execute a Fragment and print it right away (during the execution phase of the reporter)
   */
  override def executeFragment(implicit arguments: Arguments): Function[Fragment, ExecutedFragment] = (f: Fragment) => {
    val executed = super.executeFragment(arguments)(f)
    if (arguments.report.streaming) {
      handleFragment(arguments)(executed)
      printLines(Seq(executed) filter { e => isExecutedText(e) || isExecutedResult(e) }).print(new TestInterfaceResultOutput(loggers))
    }
    executed
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
  // do nothing because sbt already displays the specification name
  override def printSpecStart(message: String, stats: Stats)(implicit args: Arguments) = ()
  override def printFailure(message: String)(implicit args: Arguments)   = logFailure(message)
  override def printError(message: String)(implicit args: Arguments)     = logError(message)
  override def printSuccess(message: String)(implicit args: Arguments)   = logInfo(message)
  override def printStats(message: String)(implicit args: Arguments)     = logInfo(message)
  override def printLine(message: String)(implicit args: Arguments)      = logInfo(message)
  override def printText(message: String)(implicit args: Arguments)      = logInfo(message)
}

/**
 * This "TestInterface" result output only prints the last statistics during the export phase
 */
class TestInterfaceStatsOnlyResultOutput(override val loggers: Array[Logger]) extends TestInterfaceResultOutput(loggers) {
  override def printFailure(message: String)(implicit args: Arguments)   = ()
  override def printError(message: String)(implicit args: Arguments)     = ()
  override def printSuccess(message: String)(implicit args: Arguments)   = ()
  override def printText(message: String)(implicit args: Arguments)      = ()
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