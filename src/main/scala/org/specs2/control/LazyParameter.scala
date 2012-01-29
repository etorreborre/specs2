package org.specs2
package control
import Exceptions._

/**
 * This trait can be used to allow some function to be called with varargs, with values being
 * evaluated lazily:<code>
 *
 *   def method[T](values: LazyParameter[T]*) = {
 *     values.toStream // use the toStream method to consume the values lazily
 *   }
 *   // usage
 *   method(exp1, exp2, exp3)
 * </code>
 *
 * Note that the values are really evaluated once, unlike a by-name parameter.
 */
trait LazyParameters {
  /** transform a value to a zero-arg function returning that value */
  implicit def lazyfy[T](value: =>T): LazyParameter[T] = new LazyParameter(() => value)
}
object LazyParameters extends LazyParameters

/** class holding a value to be evaluated lazily */
class LazyParameter[+T](private val v: () => T) {
  private lazy val evaluated = v.apply()
  /**
   * @return the evaluated value. This method is private to specs2 to avoid the implicit to leak to client
   *         specifications, if the user has defined a 'value' method somewhere in his code
   */
  private[specs2] def value = evaluated

  override def toString = tryOrElse(value.toString)("Evaluation error")
  override def equals(o: Any) = value == o
  override def hashCode = value.hashCode

  def map[S >: T](f: T => S) = LazyParameters.lazyfy(f(v()))
}

