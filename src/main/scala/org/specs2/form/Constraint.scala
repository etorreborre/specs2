package org.specs2
package form
import matcher._
import execute._
/**
 * Base class for constraints executed on an expected value.
 * 
 * Subclasses include MatcherConstraint (uses a matcher), FunctionConstraint (uses a function), AnyConstraint (uses a block.)
 */
trait Constraint[T] {
  def execute(expected: Option[T]): Option[Result]
}
/**
 * This general constraint uses a function taking an actual valuen and an expected value to do the match.
 */
case class FunctionConstraint[T, S](actual: T, executor: (T, T) => Result) extends Constraint[T]  {
  def execute(expected: Option[T]) = expected match {
    case Some(e) => Some(executor(actual, e))
    case _ => None
  }
}
