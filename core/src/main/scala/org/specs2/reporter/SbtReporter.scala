package org.specs2
package reporter

import sbt.testing._
import main.Arguments
import text._
import Trim._
import time._
import AnsiColors._
import org.specs2.execute.{ Success, Failure, Error, Skipped, Pending, DecoratedResult }
import specification._
import scalaz.Scalaz._

/**
 * Reporter for the test interface defined for sbt
 * 
 * It prints out the result to the output defined by the sbt loggers
 * and publishes events to sbt event handlers
 */
class SbtConsoleReporter(consoleExporter: Option[Exporting], otherExporters: Arguments => Seq[Exporting]) extends ConsoleReporter with AllExporting {
  override def exporters(accept: String => Boolean)(implicit arguments: Arguments): Seq[Exporting] =
    otherExporters(arguments)

  override def exportConsole(accept: String => Boolean) (implicit arguments: Arguments) =
    consoleExporter
}

/**
 * This reporter will just notify the test interface about test results for the end statistics
 *
 * It is only used if we are not using the Console exporter
 */
case class FinalResultsExporter(taskDef: TaskDef,
                                handler: EventHandler,
                                loggers: Array[Logger]) extends SbtExporter(taskDef, handler, loggers) {
  override def export(implicit args: Arguments): ExecutingSpecification => ExecutedSpecification = (spec: ExecutingSpecification) => {
    val executed = spec.execute
    executed.fragments foreach handleFragment(args)
    executed
  }
}

class SbtExporter(taskDef: TaskDef, handler: EventHandler, loggers: Array[Logger]) extends TextExporting with Events {

  override def textOutput = new SbtResultOutput(loggers)

  override def export(implicit args: Arguments): ExecutingSpecification => ExecutedSpecification = (spec: ExecutingSpecification) => {
    super.export(args)(spec)
    val executed = spec.execute
    executed.fragments foreach handleFragment(args)
    executed
  }

  protected def handleFragment(implicit args: Arguments): ExecutedFragment => ExecutedFragment = (f: ExecutedFragment) => {
    f match {
      case ExecutedResult(description: FormattedString, result: org.specs2.execute.Result, timer: SimpleTimer, _, _) => {
        def handleResult(res: org.specs2.execute.Result) {
          res match {
            case Success(text,_)             => handler.handle(succeeded(taskDef))
            case r @ Failure(text, e, st, d) => handler.handle(failure(taskDef, args.traceFilter(r.exception)))
            case r @ Error(text, e)          => handler.handle(error(taskDef, args.traceFilter(r.exception)))
            case Skipped(text, _)            => handler.handle(skipped(taskDef))
            case Pending(text)               => handler.handle(pending(taskDef))
            case DecoratedResult(t, r)       => handleResult(r)
          }
        }
        handleResult(result)
        f
      }
      case _                                 => f
    }
  }
}

class SbtResultOutput(val loggers: Array[Logger]) extends LineLoggerOutput with SbtLoggers {
  def infoLog(msg: String)    = logInfo(loggers)(msg)
  def failureLog(msg: String) = logFailure(loggers)(msg)
  def errorLog(msg: String)   = logError(loggers)(msg)
}

trait SbtLoggers {

  def logFailure(loggers: Array[Logger])(message: String) = loggers.foreach { logger =>
    logger.error(removeColors(message, !logger.ansiCodesSupported))
  }
  def logError(loggers: Array[Logger])(message: String) = loggers.foreach { logger =>
    logger.error(removeColors(message, !logger.ansiCodesSupported))
  }
  def logInfo(loggers: Array[Logger])(message: String) = loggers.foreach { logger =>
    logger.info(removeColors(message, !logger.ansiCodesSupported))
  }
}

trait Events { outer =>
  def result(taskDef: TaskDef)(r: execute.Result): Event = {
    r match {
      case s @ execute.Success(_, _)             => succeeded(taskDef)
      case f @ execute.Failure(_,_,_,_)          => failure(taskDef, f.exception)
      case e @ execute.Error(_,_)                => error(taskDef, e.exception)
      case p @ execute.Pending(_)                => pending(taskDef)
      case k @ execute.Skipped(_,_)              => skipped(taskDef)
      case d @ execute.DecoratedResult(dec, res) => result(taskDef)(res)
    }
  }

  abstract class SpecEvent(taskDef: TaskDef, val status: Status, val throwable: OptionalThrowable = new OptionalThrowable) extends Event {
    val fullyQualifiedName = taskDef.fullyQualifiedName
    val fingerprint = taskDef.fingerprint
    val selector = taskDef.selectors.headOption.getOrElse(new SuiteSelector)
    val duration = -1L
  }
  case class error(taskDef: TaskDef, exception: Throwable) extends SpecEvent(taskDef, Status.Error, new OptionalThrowable(exception))
  case class failure(taskDef: TaskDef, exception: Throwable) extends SpecEvent(taskDef, Status.Failure, new OptionalThrowable(exception))
  case class succeeded(taskDef: TaskDef) extends SpecEvent(taskDef, Status.Success)
  case class skipped(taskDef: TaskDef)   extends SpecEvent(taskDef, Status.Skipped)
  case class pending(taskDef: TaskDef)   extends SpecEvent(taskDef, Status.Pending)
  case class ignored(taskDef: TaskDef)   extends SpecEvent(taskDef, Status.Ignored)
  case class canceled(taskDef: TaskDef)  extends SpecEvent(taskDef, Status.Canceled)
}
