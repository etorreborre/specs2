package org.specs2
package runner

import _root_.org.junit.runner.notification.RunNotifier
import _root_.org.junit.runner._
import main.{SystemProperties, Arguments}
import reporter._
import specification._
import java.io.{PrintStream, ByteArrayOutputStream}
import org.junit.internal.TextListener
import reflect.Classes
import scala.reflect.ClassTag

/**
 * The JUnitRunner class is a JUnit Runner class meant to be used with the RunWith annotation
 * to execute a specification as a JUnit suite.
 * 
 * The implementation is using a description Fold to fold the fragments into a tree
 * of Description objects and a Map relating each Description to a Fragment to execute. 
 *
 */
class JUnitRunner(klass: Class[_]) extends Runner with DefaultSelection { outer =>

  /** specification to execute */
  lazy val specification = SpecificationStructure.createSpecification(klass.getName)(propertiesArgs)
  /** selected fragments to execute */
  lazy val selected = select(args)(specification)
  /** descriptions for this specification */
  lazy val descriptions = new JUnitDescriptionsFragments(klass.getName)
  /** extract the root Description object and the examples to execute */
  lazy val DescriptionAndExamples(desc, fragmentsDescriptions) = descriptions.foldAll(selected.content.fragments)
  /** system properties */
  lazy val properties: SystemProperties = SystemProperties
  /** command line arguments, extracted from the system properties */
  lazy val propertiesArgs: Arguments = Arguments.extract(Seq(), properties)
  /** arguments for this specification */
  implicit lazy val args: Arguments = propertiesArgs <| specification.content.arguments

  /** @return a Description for the TestSuite */
  def getDescription = desc

  def run(n: RunNotifier) {
    val reporter = new JUnitReporter {
      lazy val notifier = n
      lazy val selected = outer.selected
      lazy val args = outer.args
      lazy val properties = outer.properties
      lazy val descriptions = outer.fragmentsDescriptions
    }
    reporter.report
  }
}

/**
 * Factory methods to help with testing
 */
object JUnitRunner {
  def apply[T <: SpecificationStructure](implicit m: ClassTag[T]) = new JUnitRunner(m.runtimeClass)
  def apply[T <: SpecificationStructure](s: T)(implicit m: ClassTag[T], p: SystemProperties) = new JUnitRunner(m.runtimeClass) {
    override lazy val specification = s
    override lazy val properties = p
  }
  def apply[T <: SpecificationStructure](fs: Fragments)(implicit m: ClassTag[T]) = new JUnitRunner(m.runtimeClass) {
    override lazy val specification = new Specification { def is = fs }
  }
  def apply[T <: SpecificationStructure](f: Fragments, props: SystemProperties)(implicit m: ClassTag[T]) = new JUnitRunner(m.runtimeClass) { outer =>
    override lazy val specification = new Specification { def is = f }
    override lazy val properties = props
    override def run(n: RunNotifier) = {
      val reporter = new JUnitReporter with AllExporting {
        lazy val notifier = n
        lazy val selected = outer.selected
        lazy val args = outer.args
        lazy val properties = outer.properties
        lazy val descriptions = outer.fragmentsDescriptions
      }
      reporter.report
    }
  }
}

/**
 * Simple JUnitRunner to run specifications on the console for testing
 */
object textJUnitRunner {

  def main(args: Array[String]) {
    println(run(args))
  }

  def run(args: Array[String]): String =
    if (args.isEmpty) "args must at least pass a class name"
    else              run(Classes.loadClassOf(args(0)))

  def run(klass: Class[_]): String = runSpec(klass)._2

  def runSpec(klass: Class[_]): (SpecificationStructure, String) = {
    val n = new RunNotifier
    val o = new ByteArrayOutputStream
    val r = new JUnitRunner(klass)
    val result = new Result
    val runListener = result.createListener
    n.addFirstListener(runListener)
    n.addListener(new TextListener(new PrintStream(o)))
    r.run(n)
    n.fireTestRunFinished(result)
    (r.specification, o.toString)
  }
}