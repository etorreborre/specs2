package org.specs

trait Specification {
  
  case class Examples(s: String) {
	def h1(ex: Example) = this
  }
  implicit def title(s: String) = new Examples(s)

  case class Example(s: String)(nested: List[Example]) {
	def ~[T](ex: =>T) = this
	def ^[T](ex: =>T) = this
	
	def |(a: Example*) = Example(s)(a.toList)
	def ^(a: Example*) = Example(s)(a.toList)
	def &(a: Example*) = Example(s)(a.toList)
	def \(a: Example*) = Example(s)(a.toList)
	def /(a: Example*) = Example(s)(a.toList)
	//def ->[T](a: =>T) = Example(s)(a.toList)
	def ::(a: Example*) = Example(s)(a.toList)
	def <(a: Example*) = Example(s)(a.toList)
	def <<(a: Example*) = Example(s)(a.toList)
	def >(a: Example*) = Example(s)(a.toList)
	def >>(a: Example*) = Example(s)(a.toList)
	def and(a: Example*) = Example(s)(a.toList)
	def **(a: Example*) = Example(s)(a.toList)
	def *(a: Example*) = Example(s)(a.toList)
	def $(a: Example*) = Example(s)(a.toList)
  }
  implicit def forExample(s: String) = Example(s)(Nil)
  implicit def toExamples(e: Example) = "" h1(e)
	def report(e: Any*) = e
	
	  class Reporter {
	//def |>(e: Examples) = e
  }
  object junit extends Reporter
  object console extends Reporter
  //object |> extends Reporter
object end extends Example("end")(Nil)
object | extends Example("sp")(Nil)
  trait Context {
	def before = {}
	def after = {}
  }
  object context extends Context
  implicit def toExamples(s: String): Any = new Object {
	def ~(c: Context) = new Examples(s)
  }

}