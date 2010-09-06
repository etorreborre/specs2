package org.specs2
import org.specs2.specification._
import org.specs2.runner._

trait Specification extends ExamplesBuilder with Expectations with AConsoleReporter {
  val examples: Examples
  def include(s: Specification) =  new SpecificationExamples(examples.fragments)
  def report: Unit = reporter.report(this)
  def main(args: Array[String]): Unit = report
}