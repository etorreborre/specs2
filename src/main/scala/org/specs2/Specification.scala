package org.specs2
import specification._
import runner._

trait Specification extends ExamplesBuilder with Expectations with AConsoleReporter {
  val examples: Examples
  def include(s: Specification) =  group(examples.fragments)
  def report: Unit = reporter.report(this)
  def main(args: Array[String]): Unit = report
}