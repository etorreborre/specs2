package org.specs2.specification

trait ExamplesBuilder {
  implicit def start(s: String): Examples = new Examples(List(Text(s)))
  implicit def text(s: String): Text = new Text(s)
  implicit def forExample(s: String): ExampleDesc = new ExampleDesc(s)
  class ExampleDesc(s: String) {
	def ~[T](t: =>T) = new Example(s, body = Some(() => t))
  }
}
  trait Fragment	
  case class Text(t: String) extends Fragment
  case class Examples(fragments: List[Fragment]) {
	override def toString = fragments.mkString("\n")
	def ^(e: Fragment) = copy(fragments = this.fragments :+ e) 
	def ^(e: List[Fragment]) = copy(fragments = this.fragments ++ e) 
  }
  case class Example(desc: String = "", body: Option[()=>Any] = None) extends Fragment { 
	def ^(a: Example) = Examples(List(this))
  }
