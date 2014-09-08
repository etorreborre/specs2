package org.specs2
package reporter

import junit.framework.AssertionFailedError

import scalaz.stream.{Process, Sink}
import text.NotNullStrings._
import scalaz.concurrent.Task
import org.junit.runner.Description
import org.junit.runner.notification.RunNotifier
import specification.core._
import text.AnsiColors
import execute._
import org.junit.ComparisonFailure
import main.Arguments
import control.Throwablex
import scalaz.stream.io
import data.Fold

/**
 * The JUnitPrinter sends notifications to JUnit's RunNotifier
 */
trait JUnitPrinter extends Printer { outer =>

  /** the junit notifier to use */
  def notifier: RunNotifier

  /** the descriptions of the specification fragments */
  def descriptions: Map[Fragment, Description]

  /** description for the whole specification */
  def description: Description

  def fold(env: Env, spec: SpecStructure) = Fold.fromSink(sink(spec, env))

  def sink(spec: SpecStructure, env: Env): Sink[Task, Fragment] = {
    val acquire = Task.now {
      notifier.fireTestRunStarted(description); notifier
    }
    val shutdown = (notifier: RunNotifier) => Task.now(notifier.fireTestRunFinished(new org.junit.runner.Result))
    val step = (notifier: RunNotifier) => Task.now(notifyJUnit(env.arguments))

    io.resource(acquire)(shutdown)(step)
  }

  def notifyJUnit(args: Arguments): Fragment => Task[Unit] = { fragment =>
    // find the fragment with the same description and same location
    val description = descriptions.find { case (f, d) =>
      f.description == fragment.description && f.location == fragment.location }.map(_._2)

    description.map { description: Description =>
      if (fragment.isExecutable) Task.now {
        notifier.fireTestStarted(description)
        notifyResult(description, fragment.executionResult)(args)
      } else Task.now(())
    }.getOrElse(Task.now(()))
  }

  private def notifyResult(description: Description, result: Result)(implicit args: Arguments) =
    result match {
      case f @ Failure(m, e, st, d)                     => failWith(description, junitFailure(f))
      case e @ Error(m, st)                             => failWith(description, args.traceFilter(e.exception))
      case DecoratedResult(_, f @ Failure(m, e, st, d)) => failWith(description, junitFailure(f))
      case DecoratedResult(_, e @ Error(m, st))         => failWith(description, args.traceFilter(e.exception))
      case Pending(_) | Skipped(_, _)                   => notifier.fireTestIgnored(description)
      case Success(_, _) | DecoratedResult(_, _)        => notifier.fireTestFinished(description)
    }

  private def failWith(description: Description, failure: Throwable) = {
    notifier.fireTestFailure(new org.junit.runner.notification.Failure(description, failure))
    notifier.fireTestFinished(description)
  }

  /** @return a Throwable expected by JUnit Failure object */
  private def junitFailure(f: Failure)(implicit args: Arguments): Throwable = f match {
    case Failure(m, e, st, NoDetails()) =>
      new SpecFailureAssertionFailedError(Throwablex.exception(AnsiColors.removeColors(m), args.traceFilter(st)))

    case Failure(m, e, st, FailureDetails(expected, actual)) => new ComparisonFailure(AnsiColors.removeColors(m), expected, actual) {
      private val e = args.traceFilter(f.exception)
      override def getStackTrace = e.getStackTrace
      override def getCause = e.getCause
      override def printStackTrace() { e.printStackTrace() }
      override def printStackTrace(w: java.io.PrintStream) { e.printStackTrace(w) }
      override def printStackTrace(w: java.io.PrintWriter) { e.printStackTrace(w) }
    }
  }

}

/**
 * This class refines the `AssertionFailedError` from junit
 * and provides the stackTrace of an exception which occurred during the specification execution
 */
class SpecFailureAssertionFailedError(e: Exception) extends AssertionFailedError(e.getMessage.notNull) {
  override def toString = e.toString
  override def getStackTrace = e.getStackTrace
  override def getCause = e.getCause
  override def printStackTrace() { e.printStackTrace() }
  override def printStackTrace(w: java.io.PrintStream) { e.printStackTrace(w) }
  override def printStackTrace(w: java.io.PrintWriter) { e.printStackTrace(w) }
}

