package org.specs2
package reporter

import data.Fold
import org.specs2.control.{Actions, Action}
import org.specs2.execute.Details
import scalaz.concurrent.Task
import scalaz.stream._
import sbt.testing._
import Fold._
import org.specs2.text.AnsiColors._
import scalaz.stream.io._
import main.Arguments
import specification.core._
import SbtPrinter._

/**
 * Text printer for Sbt
 *
 * It delegates the console printing to a normal text printer but using the Sbt loggers
 * It also publishes events (success, error, skipped, pending) to Sbt
 */
trait SbtPrinter extends Printer {
  def prepare(env: Env, specifications: List[SpecificationStructure]): Action[Unit] = Actions.unit

  def finalize(env: Env, specifications: List[SpecificationStructure]): Action[Unit] = Actions.unit

  /** sbt loggers to display text */
  def loggers: Array[Logger]

  /** events handler to notify Sbt of successes/failures */
  def events: SbtEvents

  lazy val textPrinter = TextPrinter

  def sbtNotifierPrinter(args: Arguments): Printer =
    NotifierPrinter.printer(sbtNotifier(events, args))

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
    sbtNotifierPrinter(env.arguments).fold(env, spec)
}

object SbtPrinter {
  def sbtNotifier(events: SbtEvents, args: Arguments) = new Notifier {
    private val context: scala.collection.mutable.Stack[String] =
      new scala.collection.mutable.Stack[String]

    private def inContext(name: String): String =
      (context.toVector :+ name).mkString("::")

    def specStart(title: String, location: String): Unit = ()
    def specEnd(title: String, location: String): Unit = ()

    def contextStart(text: String, location: String): Unit =
    { context.push(text); Unit }

    def contextEnd(text: String, location: String): Unit =
    { if (!context.isEmpty) context.pop; () }

    def text(text: String, location: String): Unit = ()
    def exampleStarted(name: String, location: String): Unit = ()

    def exampleSuccess(name: String, duration: Long): Unit =
      events.succeeded(inContext(name), duration)

    def exampleFailure(name: String, message: String, location: String, f: Throwable, details: Details, duration: Long): Unit =
      events.failure(inContext(name), duration, args.traceFilter(f))

    def exampleError(name: String, message: String, location: String, f: Throwable, duration: Long): Unit =
      events.error(inContext(name), duration, args.traceFilter(f))

    def exampleSkipped(name: String, message: String, location: String, duration: Long): Unit =
      events.skipped(inContext(name), duration)

    def examplePending(name: String, message: String, location: String, duration: Long): Unit =
      events.pending(inContext(name), duration)
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

  def suiteError()                                = handler.handle(SpecSuiteEvent(Status.Error))
  def suiteError(exception: Throwable)            = handler.handle(SpecSuiteEvent(Status.Error, new OptionalThrowable(exception)))

  def error    (name: String, durationInMillis: Long, exception: Throwable) = handler.handle(SpecTestEvent(name, Status.Error   , Some(durationInMillis), new OptionalThrowable(exception)))
  def failure  (name: String, durationInMillis: Long, exception: Throwable) = handler.handle(SpecTestEvent(name, Status.Failure , Some(durationInMillis), new OptionalThrowable(exception)))
  def succeeded(name: String, durationInMillis: Long)                       = handler.handle(SpecTestEvent(name, Status.Success , Some(durationInMillis)))
  def skipped  (name: String, durationInMillis: Long)                       = handler.handle(SpecTestEvent(name, Status.Skipped , Some(durationInMillis)))
  def pending  (name: String, durationInMillis: Long)                       = handler.handle(SpecTestEvent(name, Status.Pending , Some(durationInMillis)))
  def ignored  (name: String, durationInMillis: Long)                       = handler.handle(SpecTestEvent(name, Status.Ignored , Some(durationInMillis)))
  def canceled (name: String)                                               = handler.handle(SpecTestEvent(name, Status.Canceled, None))

  case class SpecTestEvent(name: String, status: Status, durationInMillis: Option[Long], throwable: OptionalThrowable = new OptionalThrowable) extends Event {
    val fullyQualifiedName = taskDef.fullyQualifiedName
    val fingerprint        = taskDef.fingerprint
    val selector           = new TestSelector(name)
    val duration           = durationInMillis.getOrElse(-1L)
  }

  case class SpecSuiteEvent(status: Status, throwable: OptionalThrowable = new OptionalThrowable) extends Event {
    val fullyQualifiedName = taskDef.fullyQualifiedName
    val fingerprint        = taskDef.fingerprint
    val selector           = new SuiteSelector
    val duration           = -1L
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
