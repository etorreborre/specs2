package org.specs2
package runner

import _root_.org.junit.runner.notification.RunNotifier
import _root_.org.junit.runner._
import main.{SystemProperties, Arguments}
import reporter._
import specification._

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
  lazy val specification = SpecificationStructure.createSpecification(klass.getName)(args)
  /** selected fragments to execute */
  lazy val selected = select(args)(specification)
  /** descriptions for this specification */
  lazy val descriptions = new JUnitDescriptionsFragments(klass.getName)
  /** extract the root Description object and the examples to execute */
  lazy val DescriptionAndExamples(desc, fragmentsDescriptions) = descriptions.foldAll(selected.content.fragments)
  /** system properties */
  lazy val properties: SystemProperties = SystemProperties
  /** command line arguments, extracted from the system properties*/
  implicit lazy val args: Arguments = Arguments.extract(Seq(), properties)

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
  def apply[T <: SpecificationStructure](implicit m: ClassManifest[T]) = new JUnitRunner(m.erasure)
  def apply[T <: SpecificationStructure](s: T)(implicit m: ClassManifest[T], p: SystemProperties) = new JUnitRunner(m.erasure) {
    override lazy val specification = s
    override lazy val properties = p
  }
  def apply[T <: SpecificationStructure](fs: Fragments)(implicit m: ClassManifest[T]) = new JUnitRunner(m.erasure) {
    override lazy val specification = new Specification { def is = fs }
  }
  def apply[T <: SpecificationStructure](f: Fragments, props: SystemProperties, console: TextExporting, html: HtmlExporting)(implicit m: ClassManifest[T]) = new JUnitRunner(m.erasure) { outer =>
    override lazy val specification = new Specification { def is = f }
    override lazy val properties = props
    override def run(n: RunNotifier) = {
      val reporter = new JUnitReporter {
        lazy val notifier = n
        lazy val selected = outer.selected
        lazy val args = outer.args
        lazy val properties = outer.properties
        lazy val descriptions = outer.fragmentsDescriptions
        override def exporters(accept: String => Boolean)(implicit arguments: Arguments): Seq[Exporting] = Seq(console, html)
      }
      reporter.report
    }
  }
}
