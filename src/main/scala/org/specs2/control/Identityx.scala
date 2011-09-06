package org.specs2
package control

/**
 * This class provides the possibility to execute a function on an object if a condition is true
 * If not, the object is returned
 */
private[specs2]
class Identityx[T](t: =>T) {
  def ?>(f: T => T)(implicit condition: Boolean = true) = when(condition)(f)
	def when(condition: Boolean)(f: T => T) = if (condition) f(t) else t
}

private[specs2]
object Identityx {
  implicit def identityx[T](f: =>T): Identityx[T] = new Identityx(f)
}