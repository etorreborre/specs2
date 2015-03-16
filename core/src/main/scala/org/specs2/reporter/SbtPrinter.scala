package org.specs2
package reporter

import data.Fold
import org.specs2.control.{Actions, Action}
import scalaz.concurrent.Task
import scalaz.stream._
import sbt.testing._
import Fold._
import org.specs2.text.AnsiColors._
import scalaz.stream.io._
import main.Arguments
import specification.core._

/**
 * Text printer for Sbt
 *
 * It delegates the console printing to a normal text printer but using the Sbt loggers
 * It also publishes events (success, error, skipped, pending) to Sbt
 */
trait SbtPrinter extends Printer {
  def prepare(env: Env, specifications: List[SpecificationStructure]): Action[Unit]  = Actions.unit
  def finalize(env: Env, specifications: List[SpecificationStructure]): Action[Unit] = Actions.unit

  /** sbt loggers to display text */
  def loggers: Array[Logger]
  /** events handler to notify Sbt of successes/failures */
  def events: SbtEvents

  lazy val textPrinter = TextPrinter

  /**
   * use 2 Folds:
   * - one for logging messages to the console
   * - one for registering sbt events
   */
  def fold(env: Env, spec: SpecStructure): Fold[Fragment] =
    textFold(env, spec) >> eventFold(env, spec)

  def textFold(env: Env, spec: SpecStructure) =
    textPrinter.fold(env.setLineLogger(SbtLineLogger(loggers)), spec)

  def eventFold(env: Env, spec: SpecStructure) =
    Fold.fromSink(eventSink(env, spec))

  def eventSink(env: Env, spec: SpecStructure): Sink[Task, Fragment] =
    channel(notify(env.arguments))

  def notify(args: Arguments): Fragment => Task[Unit] = { fragment =>
    import org.specs2._
    Task.now {
      if (fragment.isExecutable) {
        def handleResult(res: execute.Result) {
          res match {
            case execute.Success(text,_)             => events.succeeded()
            case r @ execute.Failure(text, e, st, d) => events.failure(args.traceFilter(r.exception))
            case r @ execute.Error(text, e)          => events.error(args.traceFilter(r.exception))
            case execute.Skipped(text, _)            => events.skipped()
            case execute.Pending(text)               => events.pending()
            case execute.DecoratedResult(t, r)       => handleResult(r)
          }
        }
        handleResult(fragment.executionResult)
      }
    }
  }

}

/**
 * Sbt events for a given TaskDef and event handler
 */
trait SbtEvents {
  /** sbt event handler to notify of successes/failures */
  def handler: EventHandler
  /** sbt task definition for this run */
  def taskDef: TaskDef

  def error()                        = handler.handle(SpecEvent(Status.Error))
  def error(exception: Throwable)    = handler.handle(SpecEvent(Status.Error, new OptionalThrowable(exception)))
  def failure(exception: Throwable)  = handler.handle(SpecEvent(Status.Failure, new OptionalThrowable(exception)))
  def succeeded()                    = handler.handle(SpecEvent(Status.Success))
  def skipped  ()                    = handler.handle(SpecEvent(Status.Skipped))
  def pending  ()                    = handler.handle(SpecEvent(Status.Pending))
  def ignored  ()                    = handler.handle(SpecEvent(Status.Ignored))
  def canceled ()                    = handler.handle(SpecEvent(Status.Canceled))

  case class SpecEvent(status: Status, throwable: OptionalThrowable = new OptionalThrowable) extends Event {
    val fullyQualifiedName = taskDef.fullyQualifiedName
    val fingerprint        = taskDef.fingerprint
    val selector           = taskDef.selectors.headOption.getOrElse(new SuiteSelector)
    val duration           = 10000L
  }
}

/**
 * Line logger using sbt's loggers
 */
case class SbtLineLogger(loggers: Array[Logger]) extends BufferedLineLogger {
  def infoLine(msg: String) = loggers.foreach { logger =>
    logger.info(removeColors(msg, !logger.ansiCodesSupported))
  }
  /** failures are represented as errors in sbt */
  def failureLine(msg: String) = errorLine(msg)

  def errorLine(msg: String) = loggers.foreach { logger =>
    val msg1 = removeColors(msg, !logger.ansiCodesSupported)
    logger.error(msg1)
  }
}
