package org.specs2
package reporter

import junit.framework.AssertionFailedError
import text.NotNullStrings._
import org.junit.runner.Description
import org.junit.runner.notification.RunNotifier
import specification.core._
import text.AnsiColors
import execute._
import org.junit.ComparisonFailure
import main.Arguments
import control._
import origami._
import control.ExecutionOrigin._
import org.specs2.fp.syntax._

/**
 * The JUnitPrinter sends notifications to JUnit's RunNotifier
 */
trait JUnitPrinter extends Printer { outer =>
  def prepare(env: Env, specifications: List[SpecStructure]): Action[Unit] = Actions.unit
  def finalize(env: Env, specifications: List[SpecStructure]): Action[Unit] = Actions.unit

  /** the junit notifier to use */
  def notifier: RunNotifier

  /** the descriptions of the specification fragments */
  def descriptions: Map[Fragment, Description]

  /** description for the whole specification */
  def description: Description

  // test run start and finish must not be notified if we execute the test from
  // the JUnitCore runner because it is already doing that.
  // Otherwise this could lead to double reporting see #440
  def sink(env: Env, spec: SpecStructure) =
    fold.bracket[ActionStack, Fragment, RunNotifier](
      open = Actions.protect { if (!isExecutedFromJUnitCore) notifier.fireTestRunStarted(description); notifier })(
      step = (notifier: RunNotifier, fragment: Fragment) => notifyJUnit(env.arguments)(fragment).as(notifier))(
      close = (notifier: RunNotifier) => Actions.protect(if (!isExecutedFromJUnitCore) notifier.fireTestRunFinished(new org.junit.runner.Result) else ())
    )

  def notifyJUnit(args: Arguments): Fragment => Action[Unit] = { fragment =>
    // find the fragment with the same description and same location
    val description = descriptions.find { case (f, d) =>
      f.description == fragment.description && f.location == fragment.location }.map(_._2)

    fragment.executionResult.map { result =>
      description.map { description: Description =>
        if (fragment.isExecutable) {
          notifier.fireTestStarted(description)
          notifyResult(description, result)(args)
        } else ()
      }.getOrElse(())
    }
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
    case Failure(m, e, st, NoDetails) =>
      new SpecFailureAssertionFailedError(Throwablex.exception(AnsiColors.removeColors(m), args.traceFilter(st)))

    case Failure(m, e, st, FromNotImplementedError) =>
      new SpecFailureAssertionFailedError(Throwablex.exception(AnsiColors.removeColors(m), args.traceFilter(st)))

    case Failure(m, e, st, FromJUnitAssertionError) =>
      new SpecFailureAssertionFailedError(Throwablex.exception(AnsiColors.removeColors(m), args.traceFilter(st)))

    case Failure(m, e, st, FailureDetails(actual, expected)) => new ComparisonFailure(AnsiColors.removeColors(m), expected, actual) {
      private val e = args.traceFilter(f.exception)
      override def getStackTrace = e.getStackTrace
      override def getCause = e.getCause
      override def printStackTrace() { e.printStackTrace() }
      override def printStackTrace(w: java.io.PrintStream) { e.printStackTrace(w) }
      override def printStackTrace(w: java.io.PrintWriter) { e.printStackTrace(w) }
    }

    case Failure(m, e, st, FailureSeqDetails(actual, expected)) =>
      val details =
        if (args.diffs.showSeq(actual, expected, ordered = true)) {
          val (added, missing) = args.diffs.showSeqDiffs(actual, expected, ordered = true)
          List(showValues("Added", added), showValues("Missing", missing)).mkString(" / ")
        } else ""

      new ComparisonFailure(AnsiColors.removeColors(m+details), expected.mkString("\n"), actual.mkString("\n")) {
        private val e = args.traceFilter(f.exception)
        override def getStackTrace = e.getStackTrace
        override def getCause = e.getCause
        override def printStackTrace() { e.printStackTrace() }
        override def printStackTrace(w: java.io.PrintStream) { e.printStackTrace(w) }
        override def printStackTrace(w: java.io.PrintWriter) { e.printStackTrace(w) }
      }

    case Failure(m, e, st, details @ FailureSetDetails(actual, expected)) =>
      val details =
        if (args.diffs.showSeq(actual.toSeq, expected.toSeq, ordered = false)) {
          val (added, missing) = args.diffs.showSeqDiffs(actual.toSeq, expected.toSeq, ordered = false)
          List(showValues("Added", added.toSeq), showValues("Missing", missing.toSeq)).mkString(" / ")
        } else ""

      new ComparisonFailure(AnsiColors.removeColors(m+details), expected.mkString("\n"), actual.mkString("\n")) {
        private val e = args.traceFilter(f.exception)
        override def getStackTrace = e.getStackTrace
        override def getCause = e.getCause
        override def printStackTrace() { e.printStackTrace() }
        override def printStackTrace(w: java.io.PrintStream) { e.printStackTrace(w) }
        override def printStackTrace(w: java.io.PrintWriter) { e.printStackTrace(w) }
      }

    case Failure(m, e, st, details @ FailureMapDetails(actual, expected)) =>
      val details =
        if (args.diffs.showMap(actual, expected)) {
          val (added, missing, different) = args.diffs.showMapDiffs(actual, expected)
          List(showValues("Added", added), showValues("Missing", missing), showValues("Different", different)).mkString(" / ")
        } else ""

      new ComparisonFailure(AnsiColors.removeColors(m+details), expected.mkString("\n"), actual.mkString("\n")) {
        private val e = args.traceFilter(f.exception)
        override def getStackTrace = e.getStackTrace
        override def getCause = e.getCause
        override def printStackTrace() { e.printStackTrace() }
        override def printStackTrace(w: java.io.PrintStream) { e.printStackTrace(w) }
        override def printStackTrace(w: java.io.PrintWriter) { e.printStackTrace(w) }
      }
  }

  /** show values as a string with a description */
  def showValues(description: String, values: Seq[Any]): String =
    if (values.nonEmpty) s"$description ${values.map(notNullPair).mkString("\n", "\n", "\n\n")}" else ""

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

