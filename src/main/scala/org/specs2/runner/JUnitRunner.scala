package org.specs2
package runner

import _root_.org.junit.runner.notification.RunNotifier
import _root_.org.junit._
import _root_.org.junit.runner._
import junit.framework.AssertionFailedError
import org.specs2.internal.scalaz.Scalaz
import Scalaz._
import main.{ Arguments, SystemProperties }
import reflect.Classes._
import execute._
import reporter._
import specification._
import text.AnsiColors
import control.{ExecutionOrigin, Throwablex}

/**
 * The JUnitRunner class is a junit Runner class meant to be used with the RunWith annotation
 * to execute a specification as a JUnit suite.
 * 
 * The implementation is using a description Fold to fold the fragments into a tree
 * of Description objects and a Map relating each Description to a Fragment to execute. 
 *
 */
class JUnitRunner(klass: Class[_]) extends Runner with ExecutionOrigin with DefaultSelection with DefaultSequence with Exporters {

  private val executor = new FragmentExecution {}
  
  /** specification to execute */
  protected lazy val specification = SpecificationStructure.createSpecification(klass.getName)(commandLineArgs)

  /** command line arguments*/
  lazy val commandLineArgs: Arguments = Arguments.extract(Seq(), properties)
  /** arguments for the specification */
  implicit lazy val args: Arguments = commandLineArgs <| specification.content.arguments
  /** fold object used to create descriptions */
  private val descriptions = new JUnitDescriptionsFragments(klass.getName)
  /** extract the root Description object and the examples to execute */
  private lazy val DescriptionAndExamples(desc, descriptedFragments) = descriptions.foldAll((select(args)(specification) |> sequence).fragments)
  /** system properties */
  protected lazy val properties: SystemProperties = SystemProperties
  /** @return a Description for the TestSuite */
  def getDescription = desc

  /** 
   * run the suite by executing each fragment related to a description:
   * - execute all fragments (including Steps which are reported as steps)
   * - for each result, report the failure/error/skipped or pending message as a
   *   junit failure or ignored event on the RunNotifier
   */
  def run(notifier: RunNotifier) {
    descriptedFragments |> notifyJUnit(notifier) |> export
  }

  /**
   * for each pair (description, fragment) execute the fragment and notify JUnit
   * collect the list executed fragments to pass to other reporters
   */
  private def notifyJUnit(notifier: RunNotifier) = (fragments: Seq[(Description, Fragment)]) => {
    fragments.toList collect {
      case (desc, f @ SpecStart(_,_,_)) => notifier.fireTestRunStarted(desc); executeFragment(f)
      case (desc, f @ SpecEnd(_,_))     => notifier.fireTestRunFinished(new org.junit.runner.Result); executeFragment(f)
      case (desc, f @ Text(_))          => executeFragment(f)

      case (desc, f @ Example(_,_))     => {
        notifier.fireTestStarted(desc)
        val result = executeFragment(f)
        notifyResult(notifier, desc, result)
        notifier.fireTestFinished(desc)
        result
      }
      case (desc, f @ Step(_,_))        => notifyResult(notifier, desc, executeFragment(f))
      case (desc, f @ Action(_))        => notifyResult(notifier, desc, executeFragment(f))
    }
  }

  /**
   * notify JUnit of a new result
   */
  private def notifyResult(notifier: RunNotifier, desc: Description, result: ExecutedFragment): ExecutedFragment = {
    result match {
      case ExecutedResult(_, r, timer, _,_) => {
        r match {
          case f @ Failure(m, e, st, d)                     => notifier.fireTestFailure(new notification.Failure(desc, junitFailure(f)))
          case e @ Error(m, st)                             => notifier.fireTestFailure(new notification.Failure(desc, args.traceFilter(e.exception)))
          case DecoratedResult(_, f @ Failure(m, e, st, d)) => notifier.fireTestFailure(new notification.Failure(desc, junitFailure(f)))
          case DecoratedResult(_, e @ Error(m, st))         => notifier.fireTestFailure(new notification.Failure(desc, args.traceFilter(e.exception)))
          case Pending(_) | Skipped(_, _)                   => notifier.fireTestIgnored(desc)
          case Success(_,_) | DecoratedResult(_, _)         => ()
        }
      }
      case other => ()
    }
    result
  }

  private def export = (executed: Seq[ExecutedFragment]) => {
    def exportTo = (name: String) => properties.isDefined(name) || args.contains(name)

    val executedSpecification = ExecutingSpecification.create(specification.content.specName, executed)
    exportToOthers(args, exportTo)(executedSpecification)
    executed
  }

  private def executeFragment(f: Fragment) = executor.executeFragment(args)(f)

  /** @return a Throwable expected by JUnit Failure object */
  private def junitFailure(f: Failure)(implicit args: Arguments): Throwable = f match {
    case Failure(m, e, st, NoDetails()) =>
      new SpecFailureAssertionFailedError(Throwablex.exception(AnsiColors.removeColors(m), args.traceFilter(st)))

    case Failure(m, e, st, FailureDetails(expected, actual)) => new ComparisonFailure(AnsiColors.removeColors(m), expected, actual) {
        private val e = args.traceFilter(f.exception)
        override def getStackTrace = e.getStackTrace
        override def getCause = e.getCause
        override def printStackTrace = e.printStackTrace
        override def printStackTrace(w: java.io.PrintStream) = e.printStackTrace(w)
        override def printStackTrace(w: java.io.PrintWriter) = e.printStackTrace(w)
      }
  }
}
/**
 * Factory methods to help with testing
 */
object JUnitRunner {
  def apply[T <: SpecificationStructure](implicit m: ClassManifest[T]) = new JUnitRunner(m.erasure)
  def apply[T <: SpecificationStructure](s: T)(implicit m: ClassManifest[T], p: SystemProperties) = new JUnitRunner(m.erasure) {
    override protected lazy val specification = s
    override protected lazy val properties = p
  }
  def apply[T <: SpecificationStructure](fs: Fragments)(implicit m: ClassManifest[T]) = new JUnitRunner(m.erasure) {
    override protected lazy val specification = new Specification { def is = fs }
  }
  def apply[T <: SpecificationStructure](f: Fragments, props: SystemProperties, console: TextExporting, html: HtmlExporting)(implicit m: ClassManifest[T]) = new JUnitRunner(m.erasure) {
      override protected lazy val specification = new Specification { def is = f }
      override protected lazy val properties = props
      override def exporters(accept: String => Boolean)(implicit arguments: Arguments): Seq[Exporting] = Seq(console, html)
  }
}
/**
 * This class refines the <code>AssertionFailedError</code> from junit
 * and provides the stackTrace of an exception which occurred during the specification execution
 */
class SpecFailureAssertionFailedError(e: Exception) extends AssertionFailedError(e.getMessage) {
  override def getStackTrace = e.getStackTrace
  override def getCause = e.getCause
  override def printStackTrace = e.printStackTrace
  override def printStackTrace(w: java.io.PrintStream) = e.printStackTrace(w)
  override def printStackTrace(w: java.io.PrintWriter) = e.printStackTrace(w)
}

/**
 * Descriptions for a seq of Fragments to execute
 */
class JUnitDescriptionsFragments(className: String) extends JUnitDescriptions[Fragment](className) {
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
        case s @ SpecStart(_,_,_)       => Some(createDescription(className, suiteName=testName(s.name)) -> f)
        case Text(t)                    => Some(createDescription(className, suiteName=testName(t)) -> f)
        case Example(description, body) => Some(createDescription(className, label=nodeLabel.toString, testName=testName(description.toString, parentPath(parentNodes))) -> f)
        case Step(action,_)             => Some(createDescription(className, label=nodeLabel.toString, testName="step") -> f)
        case Action(action)             => Some(createDescription(className, label=nodeLabel.toString, testName="action") -> f)
        case other                      => None
      }
  }