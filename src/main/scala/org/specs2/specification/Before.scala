package org.specs2
package specification
import control.Exceptions._

trait Before {
  def before: Any
  def apply[T <: Result](a: =>T): Result = { 
	tryo(before)(Error(_)).left.getOrElse(a)
  }
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
