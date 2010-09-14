package org.specs2
import specification._
import runner._
import matcher._

trait Specification extends ExamplesBuilder with MustExpectations with Matchers with AConsoleReporter with Main {
  val examples: Examples
  def include(s: Specification) =  group(examples.fragments)
  override def main(args: Array[String]): Unit = reporter.report(this)
}
trait Main extends Arguments {
  def main(args: Array[String]): Unit
}
trait Arguments {
  protected def args: String = ""
  private[specs2] def arguments = new Args(args)
}
case class Args(arguments: String) {
  private def names = arguments.split(" ").map(_.replace("-", "")).toList
  def contains(name: String) = names.contains(name)
  override def toString = names.mkString
}