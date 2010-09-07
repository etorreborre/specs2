package org.specs2.function

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
}