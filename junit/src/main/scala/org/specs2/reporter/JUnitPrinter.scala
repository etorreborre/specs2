package org.specs2
package reporter

import junit.framework.AssertionFailedError
import text.NotNullStrings.*
import org.junit.runner.Description
import org.junit.runner.notification.RunNotifier
import specification.core.*
import text.AnsiColors
import execute.*
import org.junit.ComparisonFailure
import main.Arguments
import control.*, ExecutionOrigin.*
import origami.*, Folds.*
import fp.syntax.*

/** The JUnitPrinter sends notifications to JUnit's RunNotifier
  */
case class JUnitPrinter(env: Env, notifier: RunNotifier) extends Printer:
  def prepare(specifications: List[SpecStructure]): Action[Unit] = Action.unit
  def finalize(specifications: List[SpecStructure]): Action[Unit] = Action.unit

  // test run start and finish must not be notified if we execute the test from
  // the JUnitCore runner because it is already doing that.
  // Otherwise this could lead to double reporting see #440
  //
  // The value should be evaluated once in the outer scope, because the
  // original stack trace is lost inside the callbacks.
  def sink(spec: SpecStructure): AsyncSink[Fragment] =
    val descriptionsTree = JUnitDescriptionsTree(spec, env.specs2ExecutionEnv)
    val description = descriptionsTree.description

    val shouldNotify = !excludeFromReporting
    bracket[Fragment, RunNotifier](open = Action.protect {
      if shouldNotify then notifier.fireTestRunStarted(description); notifier
    })(
      step = (notifier: RunNotifier, fragment: Fragment) =>
        notifyJUnit(env.arguments, descriptionsTree.descriptions)(fragment).as(notifier)
    )(
      close = (notifier: RunNotifier) =>
        Finalizer.create(if shouldNotify then notifier.fireTestRunFinished(new org.junit.runner.Result) else ())
    )

  private def notifyJUnit(args: Arguments, descriptions: Map[Fragment, Description]): Fragment => Action[Unit] = {
    fragment =>
      if Fragment.isExampleOrStep(fragment) then
        val description = findDescription(descriptions, fragment)
        fragment.executionResult.map { result =>
          description.foreach { (description: Description) =>
            if Fragment.isExample(fragment) then notifyTestResult(description, result)(using args)
            else notifyStepError(description, result)(using args)
          }
        }
      else Action.unit
  }

  private def findDescription(descriptions: Map[Fragment, Description], fragment: Fragment) =
    // find the fragment with the same description and same location
    descriptions
      .find { case (f, d) =>
        f.description == fragment.description && f.location == fragment.location
      }
      .map(_._2)

  private def notifyTestResult(description: Description, result: Result)(using args: Arguments) =
    result match
      case f @ Failure(m, e, st, d)                     => failWith(description, junitFailure(f))
      case e @ Error(m, st)                             => failWith(description, args.traceFilter(e.exception))
      case DecoratedResult(_, f @ Failure(m, e, st, d)) => failWith(description, junitFailure(f))
      case DecoratedResult(_, e @ Error(m, st))         => failWith(description, args.traceFilter(e.exception))
      case Pending(_) | Skipped(_, _)                   => notifier.fireTestIgnored(description)
      case Success(_, _) | DecoratedResult(_, _)        => successWith(description)

  private def notifyStepError(description: Description, result: Result)(using args: Arguments) =
    result match
      case f @ Failure(m, e, st, d)                     => specFailWith(description, junitFailure(f))
      case e @ Error(m, st)                             => specFailWith(description, args.traceFilter(e.exception))
      case DecoratedResult(_, f @ Failure(m, e, st, d)) => specFailWith(description, junitFailure(f))
      case DecoratedResult(_, e @ Error(m, st))         => specFailWith(description, args.traceFilter(e.exception))
      case _                                            => ()

  private def failWith(description: Description, failure: Throwable) =
    notifier.fireTestStarted(description)
    notifier.fireTestFailure(new org.junit.runner.notification.Failure(description, failure))
    notifier.fireTestFinished(description)

  private def successWith(description: Description) =
    notifier.fireTestStarted(description)
    notifier.fireTestFinished(description)

  private def specFailWith(description: Description, failure: Throwable) =
    notifier.fireTestFailure(new org.junit.runner.notification.Failure(description, failure))

  /** @return a Throwable expected by JUnit Failure object */
  private def junitFailure(f: Failure)(using args: Arguments): Throwable = f match
    case Failure(m, e, st, NoDetails) =>
      new SpecFailureAssertionFailedError(Throwablex.exception(AnsiColors.removeColors(m), args.traceFilter(st)))

    case Failure(m, e, st, FromNotImplementedError) =>
      new SpecFailureAssertionFailedError(Throwablex.exception(AnsiColors.removeColors(m), args.traceFilter(st)))

    case Failure(m, e, st, FromJUnitAssertionError) =>
      new SpecFailureAssertionFailedError(Throwablex.exception(AnsiColors.removeColors(m), args.traceFilter(st)))

    case Failure(m, e, st, FromExpectationError) =>
      new SpecFailureAssertionFailedError(Throwablex.exception(AnsiColors.removeColors(m), args.traceFilter(st)))

    case Failure(m, e, st, FailureDetailsMessages(_)) =>
      new SpecFailureAssertionFailedError(Throwablex.exception(AnsiColors.removeColors(m), args.traceFilter(st)))

    case Failure(m, e, st, FromTimeoutException) =>
      new SpecFailureAssertionFailedError(Throwablex.exception(AnsiColors.removeColors(m), args.traceFilter(st)))

    case Failure(m, e, st, FailureDetails(actual, expected)) =>
      new ComparisonFailure(AnsiColors.removeColors(m), expected, actual) {
        private val e = args.traceFilter(f.exception)
        override def getStackTrace = e.getStackTrace
        override def getCause = e.getCause
        override def printStackTrace(): Unit = { e.printStackTrace() }
        override def printStackTrace(w: java.io.PrintStream): Unit = { e.printStackTrace(w) }
        override def printStackTrace(w: java.io.PrintWriter): Unit = { e.printStackTrace(w) }
      }

  /** show values as a string with a description */
  def showValues(description: String, values: Seq[Any]): String =
    if values.nonEmpty then s"$description ${values.map(notNullPair).mkString("\n", "\n", "\n\n")}" else ""

/** This class refines the `AssertionFailedError` from junit and provides the stackTrace of an exception which occurred
  * during the specification execution
  */
class SpecFailureAssertionFailedError(e: Exception) extends AssertionFailedError(e.getMessage.notNull):
  override def toString = e.toString
  override def getStackTrace = e.getStackTrace
  override def getCause = e.getCause
  override def printStackTrace(): Unit = { e.printStackTrace() }
  override def printStackTrace(w: java.io.PrintStream): Unit = { e.printStackTrace(w) }
  override def printStackTrace(w: java.io.PrintWriter): Unit = { e.printStackTrace(w) }
