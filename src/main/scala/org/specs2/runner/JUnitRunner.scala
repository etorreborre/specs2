package org.specs2
package runner

import _root_.org.junit.runner.notification.RunNotifier
import _root_.org.junit.runners._
import _root_.org.junit._
import _root_.org.junit.runner._
import junit.framework.AssertionFailedError
import org.specs2.internal.scalaz.Scalaz
import Scalaz._
import main.{ Arguments, SystemProperties }
import io._
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
class JUnitRunner(klass: Class[_]) extends Runner with ExecutionOrigin with DefaultSelection with DefaultSequence {

  private val executor = new FragmentExecution {}
  
  /** specification to execute */
  protected lazy val specification = tryToCreateObject[SpecificationStructure](klass.getName).get

  /** arguments for the specification */
  implicit lazy val args: Arguments = specification.content.arguments
  /** fold object used to create descriptions */
  private val descriptions = new JUnitDescriptionsFragments(klass.getName)
  /** extract the root Description object and the examples to execute */
  private lazy val DescriptionAndExamples(desc, executions) = descriptions.foldAll((select(args)(specification) |> sequence).fragments)
  /** the console exporter can be used to display results in the console */
  protected lazy val consoleExporter = new TextExporting {}
  /** the html exporter can be used to output html files */
  protected lazy val htmlExporter = new HtmlExporting {}
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
    executeSpecification |> export |> notifyJUnit(notifier)
  }

  private def executeSpecification =
    executions.collect {
      case (desc, f @ SpecStart(_,_,_,_)) => (desc, executor.executeFragment(args)(f))
      case (desc, f @ Example(_, _))      => (desc, executor.executeFragment(args)(f))
      case (desc, f @ Text(_))            => (desc, executor.executeFragment(args)(f))
      case (desc, f @ Step(_))            => (desc, executor.executeFragment(args)(f))
      case (desc, f @ Action(_))          => (desc, executor.executeFragment(args)(f))
      case (desc, f @ SpecEnd(_))         => (desc, executor.executeFragment(args)(f))
    }

  private def export = (executed: Seq[(Description, ExecutedFragment)]) => {
    val commandLineArgs = properties.getProperty("commandline").getOrElse("").split("\\s")
    val arguments = Arguments(commandLineArgs:_*) <| args
    def exportTo(name: String) = properties.isDefined(name) || commandLineArgs.contains(name)
    
    if (exportTo("console")) 
      consoleExporter.export(arguments)(ExecutingSpecification.create(specification.content.specName, executed.map(_._2)))
    if (exportTo("html")) 
      htmlExporter.export(arguments)(ExecutingSpecification.create(specification.content.specName, executed.map(_._2)))

    executed
  }

  private def notifyJUnit(notifier: RunNotifier) = (executed: Seq[(Description, ExecutedFragment)]) => {
    executed foreach {
        case (desc, ExecutedResult(_, result, timer, _,_)) => {
          notifier.fireTestStarted(desc)
          result match {
            case f @ Failure(m, e, st, d)                     =>
              notifier.fireTestFailure(new notification.Failure(desc, junitFailure(f)))
            case e @ Error(m, st)                             =>
              notifier.fireTestFailure(new notification.Failure(desc, args.traceFilter(e.exception)))
            case DecoratedResult(_, f @ Failure(m, e, st, d)) =>
              notifier.fireTestFailure(new notification.Failure(desc, junitFailure(f)))
            case DecoratedResult(_, e @ Error(m, st))         =>
              notifier.fireTestFailure(new notification.Failure(desc, args.traceFilter(e.exception)))
            case Pending(_) | Skipped(_, _)                   => notifier.fireTestIgnored(desc)
            case Success(_) | DecoratedResult(_, _)           => ()
          }
          notifier.fireTestFinished(desc)
        }
        case (desc, ExecutedSpecStart(_,_,_)) => notifier.fireTestRunStarted(desc)
        case (desc, ExecutedSpecEnd(_,_,_))   => notifier.fireTestRunFinished(new org.junit.runner.Result)
        case (desc, _)                        => // don't do anything otherwise too many tests will be counted
      }
  }
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
  def apply[T <: SpecificationStructure](s: T)(implicit m: ClassManifest[T]) = new JUnitRunner(m.erasure) {
    override protected lazy val specification = s	  
  }
  def apply[T <: SpecificationStructure](fs: Fragments)(implicit m: ClassManifest[T]) = new JUnitRunner(m.erasure) {
    override protected lazy val specification = new Specification { def is = fs }
  }
  def apply[T <: SpecificationStructure](f: Fragments, props: SystemProperties, console: TextExporting, html: HtmlExporting)(implicit m: ClassManifest[T]) = new JUnitRunner(m.erasure) {
      override protected lazy val specification = new Specification { def is = f }
      override protected lazy val properties = props
      override protected lazy val consoleExporter = console
      override protected lazy val htmlExporter = html
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
        case s @ SpecStart(_,_,_,_)     => Some(createDescription(className, suiteName=testName(s.name)) -> f)
        case Text(t)                    => Some(createDescription(className, suiteName=testName(t)) -> f)
        case Example(description, body) => Some(createDescription(className, label=nodeLabel.toString, testName=testName(description.toString, parentPath(parentNodes))) -> f)
        case Step(action)               => Some(createDescription(className, label=nodeLabel.toString, testName="step") -> f)
        case Action(action)             => Some(createDescription(className, label=nodeLabel.toString, testName="action") -> f)
        case other                      => None
      }
  }