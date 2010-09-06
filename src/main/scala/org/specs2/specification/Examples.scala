package org.specs2.specification

trait ExamplesBuilder {
  implicit def toExamples(e: Example): Examples = new Examples(List(e))
  implicit def start(s: String): Examples = new Examples(List(Text(s)))
  implicit def text(s: String): Text = new Text(s)
  implicit def forExample(s: String): ExampleDesc = new ExampleDesc(s)
  class ExampleDesc(s: String) {
	def ~(t: =>Result) = new Example(s, body = Some(() => t))
  }
}
  trait Fragment	
  case class Text(t: String) extends Fragment
  case class SpecificationExamples(fragments: List[Fragment])
  case class Examples(fragments: List[Fragment]) {
	override def toString = fragments.mkString("\n")
	def ^(e: Fragment) = copy(fragments = this.fragments :+ e) 
	def ^(e: SpecificationExamples) = copy(fragments = this.fragments ++ e.fragments) 
  }
  case class Example(desc: String = "", body: Option[()=>Result] = None) extends Fragment { 
	def ^(a: Example) = Examples(List(this))
  }
