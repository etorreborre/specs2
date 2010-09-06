package org.specs2.specification

case class Tree[A](node: A, children: List[Tree[A]] = Nil) extends Traversable[A] {
  def foreach[U](f: A => U) = { f(node); children.foreach(_.foreach(f)) }
}
  trait ExamplesParser {
	def parse(examples: Examples): Traversable[Fragment]
  }
  trait TreeExamplesParser extends ExamplesParser {
	def parse(examples: Examples): Traversable[Fragment] = {
	  examples.fragments match {
    	case Nil => Traversable()
	 	case f :: Nil => Tree(f, Nil)
	 	case f :: other => Tree(f, Nil)
	  }
	}
  }
