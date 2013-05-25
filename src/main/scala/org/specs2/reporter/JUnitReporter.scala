package org.specs2
package reporter

import _root_.org.junit.runner.notification.RunNotifier
import _root_.org.junit._
import _root_.org.junit.runner._
import scalaz.{Reducer, Scalaz}
import Scalaz._
import main.{SystemProperties, Arguments}
import execute._
import specification._
import text.AnsiColors
import control.{ExecutionOrigin, Throwablex}
import junit.framework.AssertionFailedError

/**
 * The JUnitReporter reports a specification by using a JUnit RunNotifier
 *
 * To do so, it uses most of the execution pipeline of a normal reporter but overrides the fragments execution so as
 * to notify JUnit of the appropriate events
 */
trait JUnitReporter extends ExecutionOrigin with DefaultReporter with Exporters {

  /** the selected fragments to report */
  def selected: SpecificationStructure
  /** map providing a description for each fragment */
  def descriptions: Map[Fragment, Description]
  /** arguments for the specification */
  implicit def args: Arguments
  /** system properties */
  implicit def properties: SystemProperties

  /** the junit notifier to use */
  def notifier: RunNotifier

  /**
   * run the suite by executing each fragment related to a description:
   * - execute all fragments (including Steps which are reported as steps)
   * - for each result, report the failure/error/skipped or pending message as a
   *   junit failure or ignored event on the RunNotifier
  */
  def report {
    selected |> sequence |> execute |> store |> export
  }

  override def executeFragment(implicit arguments: Arguments): Function[Fragment, ExecutedFragment] = (fragment: Fragment) => {
    def execute(aFragment: Fragment) = super.executeFragment(arguments)(aFragment)
    // the description for a given fragment. It *should* always be found by construction
    val desc = descriptions(fragment)

    fragment match {
      case f @ Example(_, _) => {
        notifier.fireTestStarted(desc)
        val result = execute(f)
        notifyResult(desc, result)
        result
      }
      case f @ Step(_, _)         => notifyResult(desc, execute(f))
      case f @ Action(_)          => notifyResult(desc, execute(f))
      case f @ SpecStart(_, _, _) => notifier.fireTestRunStarted(desc); execute(f)
      case f @ SpecEnd(_, _)      => notifier.fireTestRunFinished(new org.junit.runner.Result); execute(f)
      case other                  => execute(other)
    }
  }

  /**
   * notify JUnit of a new result
   */
  private def notifyResult(desc: Description, result: ExecutedFragment): ExecutedFragment = {
    result match {
      case ExecutedResult(_, r, timer, _, _) => {
        r match {
          case f @ Failure(m, e, st, d)                     => failWith(desc, junitFailure(f))
          case e @ Error(m, st)                             => failWith(desc, args.traceFilter(e.exception))
          case DecoratedResult(_, f @ Failure(m, e, st, d)) => failWith(desc, junitFailure(f))
          case DecoratedResult(_, e @ Error(m, st))         => failWith(desc, args.traceFilter(e.exception))
          case Pending(_) | Skipped(_, _)                   => notifier.fireTestIgnored(desc)
          case Success(_, _) | DecoratedResult(_, _)        => notifier.fireTestFinished(desc)
        }
      }
      case other => ()
    }
    result
  }

  private def failWith(desc: Description, failure: Throwable) = {
    notifier.fireTestFailure(new notification.Failure(desc, failure))
    notifier.fireTestFinished(desc)
  }

  def export(implicit args: Arguments) = (executing: ExecutingSpecification) => {
    def exportTo = (name: String) => properties.isDefined(name) || args.contains(name)

    exportAll(args, exportTo)(executing)
    executing.executed
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
 * Descriptions for a seq of Fragments to execute
 */
class JUnitDescriptionsFragments(className: String)(implicit reducer: Reducer[Fragment, Levels[Fragment]]) extends JUnitDescriptions[Fragment](className)(reducer) {
  def initialFragment(className: String) = Text(className)
  /**
  * This function is used to map each node in a Tree[Fragment] to a pair of
  * (Description, Fragment)
  *
  * The Int argument is the numeric label of the current TreeNode being mapped.
  * It is used to create a unique description of the example to executed which is required
  * by JUnit
  */
  def mapper(className: String): (Fragment, Seq[DescribedFragment], Int) => Option[DescribedFragment] =
    (f: Fragment, parentNodes: Seq[DescribedFragment], nodeLabel: Int) => f match {
      case s @ SpecStart(_,_,_)       => Some(f -> createDescription(className, suiteName=testName(s.name)))
      case Text(t) if t.trim.nonEmpty => Some(f -> createDescription(className, suiteName=testName(t)))
      case Text(t)                    => None
      case Example(description, body) => Some(f -> createDescription(className, label=nodeLabel.toString, testName=testName(description.toString, parentPath(parentNodes))))
      case Step(action,_)             => Some(f -> createDescription(className, label=nodeLabel.toString, testName="step"))
      case Action(action)             => Some(f -> createDescription(className, label=nodeLabel.toString, testName="action"))
      case other                      => None
    }
}
/**
 * This class refines the `AssertionFailedError` from junit
 * and provides the stackTrace of an exception which occurred during the specification execution
 */
class SpecFailureAssertionFailedError(e: Exception) extends AssertionFailedError(e.getMessage) {
  override def getStackTrace = e.getStackTrace
  override def getCause = e.getCause
  override def printStackTrace() { e.printStackTrace() }
  override def printStackTrace(w: java.io.PrintStream) { e.printStackTrace(w) }
  override def printStackTrace(w: java.io.PrintWriter) { e.printStackTrace(w) }
}


