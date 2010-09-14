package org.specs2
import specification._
import runner._
import matcher._

trait Specification extends BaseSpecification with MustExpectations with Matchers with AConsoleReporter {
  def include(s: Specification) =  group(examples.fragments)
  override def main(args: Array[String]): Unit = reporter.report(this)
}
