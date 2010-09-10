package org.specs2
package specification

trait Before {
  def before: Any
  def apply[T](a: =>T) = { before; a }
}
trait First extends LazyValues {
  val first: Lazy[_]
  def apply[T](a: =>T) = { first.getValue; a }
}
trait LazyValues {
  implicit def lazyfy[T](t: =>T) = new Lazy(t)
  class Lazy[T](t: =>T) {
	private lazy val value = t
	def getValue = value
	def lazyfy = this
  }
}
object o extends First {
  val first = "".lazyfy
}
