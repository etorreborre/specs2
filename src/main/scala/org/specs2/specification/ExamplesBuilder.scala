package org.specs2
package specification
import execute._
import matcher._

trait ExamplesBuilder extends PredefinedFragments {

  implicit def asResult[T](r: MatchResult[T]) = r.toResult
  implicit def toExamples(e: Example): Examples = new Examples(List(e))
  implicit def start(s: String): Examples = new Examples(List(Text(s)))
  implicit def text(s: String): Text = new Text(s)
  implicit def forExample(s: String): ExampleDesc = new ExampleDesc(s)
  class ExampleDesc(s: String) {
	def ![T <% Result](t: =>T) = new Example(s, body = () => t)
  }
  implicit def group(examples: Examples) = Group(examples.fragments)
  implicit def group(fragments: List[Fragment]) = Group(fragments)
}
