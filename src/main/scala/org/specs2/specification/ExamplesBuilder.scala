package org.specs2
package specification
import execute._
import matcher._

trait FragmentsBuilder {

  implicit def asResult[T](r: MatchResult[T]) = r.toResult
  implicit def toFragments(e: Example): Fragments = new Fragments(List(e))
  implicit def start(s: String): Fragments = new Fragments(List(Text(s)))
  implicit def text(s: String): Text = new Text(s)
  implicit def forExample(s: String): ExampleDesc = new ExampleDesc(s)
  class ExampleDesc(s: String) {
	def ![T <% Result](t: =>T) = new Example(s, body = () => t)
  }
  implicit def group(Fragments: Fragments) = Group(Fragments.fragments)
  implicit def group(fragments: List[Fragment]) = Group(fragments)
}
