package org.specs2
package control

import Exceptions.*
import scala.util.NotGiven

/**
 * These functions can be used to allow some function to be called with varargs, with values being
 * evaluated lazily:
 * {{{
 *   def method[T](values: Lazy[T]*) = {
 *     values.to(LazyList) // use the toStream method to consume the values lazily
 *   }
 *   // usage
 *   method(exp1, exp2, exp3)
 * }}}
 *
 * Note that the values are really evaluated once, unlike a by-name parameter.
 */

trait LazyConversions:
  /** transform a value to a zero-arg function returning that value */
  implicit def lazyParameter[T](value: =>T): Lazy[T] =
    new Lazy(() => value)

trait DontConvertTo[T]

object LazyConversions extends LazyConversions

/** class holding a value to be evaluated lazily */
class Lazy[+T](private val v: () => T):
  private lazy val evaluated: T =
    v.apply()

  /**
   * @return the evaluated value. This method is private to specs2 to avoid the implicit to leak to client
   *         specifications, if the user has defined a 'value' method somewhere in his code
   */
  private[specs2] def value: T =
    evaluated

  override def toString = tryOrElse(value.toString)("Evaluation error")
  override def equals(o: Any) = value == o
  override def hashCode = value.hashCode

  def map[S >: T](f: T => S): Lazy[S] =
    new Lazy(() => f(value))

/** class holding an optional mutable value */
class Ref[T](var toOption: Option[T] = None):
  def get: T =
    toOption.get

  def set(t: T): Unit =
    toOption = Some(t)
    ()

  def update(f: T => T): Ref[T] =
    toOption = toOption.map(f)
    this

object Ref:

  def apply[T](t: T): Ref[T] =
    new Ref(Some(t))

  def empty[T]: Ref[T] =
    new Ref(None)
