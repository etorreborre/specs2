package org.specs2
package form

import execute._

/**
 * Base class for constraints executed on an optional expected value.
 */
trait Constraint[T] {
  def execute(expected: Option[T]): Option[Result]
}
/**
 * This general constraint uses a function taking an actual value and an expected value to do the match.
 */
case class FunctionConstraint[T, S](actual: T, executor: (T, T) => Result) extends Constraint[T]  {
  def execute(expected: Option[T]): Option[Result] = expected.map(executor(actual, _))
}
