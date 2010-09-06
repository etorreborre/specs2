package org.specs2.runner
import org.specs2._
import org.specs2.specification._

trait Reporter[T] extends ExamplesParser with Output with Mapper[T] {
  def report(spec: Specification): Unit = { 
	parse(spec.examples) collect mapper
  }

}
trait Mapper[T] {
  val mapper: PartialFunction[Fragment, T]
}

trait AReporter[T]
{
  val reporter: Reporter[T]
}