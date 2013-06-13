package org.specs2
package reporter

import _root_.org.scalasbt.testing._
import main.Arguments
import text._
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
class SbtConsoleReporter(consoleExporter: Option[Exporting], otherExporters: Arguments => Seq[Exporting]) extends ConsoleReporter with AllExporting

/**
 * This reporter will just notify the test interface about test results for the end statistics
 *
 * It is only used if we are not using the Console exporter
 */
case class FinalResultsExporter(override val className: String,
                                handler: EventHandler,
                                loggers: Array[Logger]) extends SbtExporter(className, handler, loggers) {
  override def export(implicit args: Arguments): ExecutingSpecification => ExecutedSpecification = (spec: ExecutingSpecification) => {
    val executed = spec.execute
    executed.fragments foreach handleFragment(args)
    executed
  }
}

class SbtExporter(val className: String, handler: EventHandler, loggers: Array[Logger]) extends TextExporting with SpecificationEvents {

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
            case Success(text,_)             => handler.handle(succeeded())
            case r @ Failure(text, e, st, d) => handler.handle(failure(args.traceFilter(r.exception)))
            case r @ Error(text, e)          => handler.handle(error(args.traceFilter(r.exception)))
            case Skipped(text, _)            => handler.handle(skipped())
            case Pending(text)               => handler.handle(skipped())
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

class SbtResultOutput(val loggers: Array[Logger]) extends TextResultOutput with SbtLoggers {
  override def printSpecStartName(message: String, stats: Stats)(implicit args: Arguments)  = logInfo(message)
  override def printSpecStartTitle(message: String, stats: Stats)(implicit args: Arguments) = logInfo(message)
  override def printFailure(message: String)(implicit args: Arguments)                      = logFailure(message)
  override def printError(message: String)(implicit args: Arguments)                        = logError(message)
  override def printSuccess(message: String)(implicit args: Arguments)                      = logInfo(message)
  override def printStats(message: String)(implicit args: Arguments)                        = logInfo(message)
  override def printLine(message: String)(implicit args: Arguments)                         = logInfo(message)
  override def printText(message: String)(implicit args: Arguments)                         = logInfo(message)
}

trait SbtLoggers {
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

trait Events { outer =>
  def result(className: String, r: execute.Result): Event = {
    val events = specEvents(className)
    r match {
      case s @ execute.Success(_, _)             => events.succeeded()
      case f @ execute.Failure(_,_,_,_)          => events.failure(f.exception)
      case e @ execute.Error(_,_)                => events.error(e.exception)
      case p @ execute.Pending(_)                => events.skipped()
      case k @ execute.Skipped(_,_)              => events.skipped()
      case d @ execute.DecoratedResult(dec, res) => outer.result(className, res)
    }
  }

  private def specEvents(name: String) = new SpecificationEvents {
    val className = name
  }

  case class error(fullyQualifiedName: String, throwable: Throwable) extends Event {
    val status = Status.Error
    val isModule = true
    val selector: Selector = new SuiteSelector
  }
}
trait SpecificationEvents { outer =>
  val className: String

  trait AnEvent extends Event {
    val fullyQualifiedName = outer.className
    val isModule = true
    val selector: Selector = new SuiteSelector
  }

  case class error(throwable: Throwable) extends AnEvent {
    def status = Status.Error
  }
  case class failure(throwable: Throwable) extends AnEvent {
    def status = Status.Failure
  }
  case class succeeded() extends AnEvent {
    def status = Status.Success
    def throwable = null
  }
  case class skipped() extends AnEvent {
    def status = Status.Skipped
    def throwable = null
  }

}
