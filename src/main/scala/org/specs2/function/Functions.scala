package org.specs2
package function

trait Functions {
  implicit def inject[T, S](function: PartialFunction[T, S]) = new Injected(function)
  class Injected[T, S](function: PartialFunction[T, S]) {
	def into[U](function2: PartialFunction[S, U]): PartialFunction[T, U] = new PartialFunction[T, U] {
	  private val results = new scala.collection.mutable.HashMap[T, S]
	  private def getResult(t: T): S = {
	 	if (!results.isDefinedAt(t))
	 	  results += t -> function(t)
	 	results(t)
	  }
	  override def isDefinedAt(t: T) = {
	 	if (!function.isDefinedAt(t))
	 	  false
	 	function2.isDefinedAt(getResult(t))
	  }
	  def apply(t: T) = function2(getResult(t))
	} 
  }
  implicit def sequence[T, S](function: PartialFunction[T, S]) = new Sequenced(function)
  class Sequenced[T, S](function: PartialFunction[T, S]) {
	def then[U](function2: PartialFunction[T, U]): PartialFunction[T, U] = new PartialFunction[T, U] {
	  override def isDefinedAt(t: T) = {
	 	function2.isDefinedAt(t)
	  }
	  def apply(t: T) = {
		if (function.isDefinedAt(t))
		  function(t)
	 	function2(t)
	  }
	} 
  }
  
  def identity[T]: PartialFunction[T, T] = { case (t: T) => Predef.identity(t) }
  
    implicit def PartialFunctionCategory: scalaz.Category[PartialFunction] = new scalaz.Category[PartialFunction] {
    def id[A] = {case a => a}
    def compose[X, Y, Z](f: PartialFunction[Y, Z], g: PartialFunction[X, Y]) = new PartialFunction[X, Z] {
      def isDefinedAt(x: X) = g.isDefinedAt(x) && f.isDefinedAt(g(x))
      def apply(x: X) = f(g(x))
    }
  }
  implicit def PartialFunctionArrow: scalaz.Arrow[PartialFunction] = new scalaz.Arrow[PartialFunction] {
    val category = PartialFunctionCategory

    def arrow[B, C](f: B => C) = {
      case b => f(b)
    }

    def first[B, C, D](a: PartialFunction[B, C]) = {
      case (b, d) if a isDefinedAt b => (a(b), d)
    }

    def second[B, C, D](a: PartialFunction[B, C]): PartialFunction[(D, B), (D, C)] = {
      case (d, b) if a isDefinedAt b => (d, a(b))
    }
  } 

}