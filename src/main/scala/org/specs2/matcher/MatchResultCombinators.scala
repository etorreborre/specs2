package org.specs2
package matcher

import control.Exceptions._
import execute.ResultLogicalCombinators
import MatchResultExecution._
/**
 * This trait provides logical operators to combine match results where potentially a MatchResult expression
 * throws an Exception
 */
private [specs2]
trait MatchResultCombinators extends MatchResultLogicalCombinators with ResultLogicalCombinators
object MatchResultCombinators extends MatchResultCombinators

private [specs2]
trait MatchResultLogicalCombinators {

  implicit def combineMatchResult[T](m: =>MatchResult[T]): MatchResultCombinator[T] = new MatchResultCombinator[T](m)
  class MatchResultCombinator[T](mr: =>MatchResult[T]) {
    lazy val result = executeEither(mr)
    lazy val expectable = result.fold(m1 => m1.expectable, m1 => m1.expectable)

    /** @return the logical or of two results */
    def or[S >: T](other: =>MatchResult[S]): MatchResult[S] = result.fold(m1 => other, m1 => new OrMatch(m1, other).evaluate)
    /** @return the logical and of two results */
    def and[S >: T](other: =>MatchResult[S]): MatchResult[S] = result.fold(m1 => expectable.check(m1), m1 => new AndMatch(m1, other).evaluate)
    /** apply the matcher and return the logical or of two results */
    def or(other: Matcher[T]): MatchResult[T] =
      tryOr {
        result.fold(m1 => m1.expectable.applyMatcher(other),
                    m1 => combineMatchResult(m1).or(execute(m1.expectable.applyMatcher(other))))
      } { e => Expectable({ throw e; expectable.value }).applyMatcher(other) }

    /** apply the matcher and return the logical and of two results */
    def and(other: Matcher[T]): MatchResult[T] =
      result.fold(m1 => m1,
                  m1 => combineMatchResult(m1).and(execute(m1.expectable.applyMatcher(other))))
    /** @return the negation of this result */
    def not: MatchResult[T] = result.fold(m1 => m1.negate, m1 => m1.negate)

    /** only consider this result if the condition is true */
    def when(b: Boolean, m: String= ""): MatchResult[T] = if (b) mr else MatchSuccess(m, m, expectable)
    /** only consider this result if the condition is false */
    def unless(b: Boolean, m: String= ""): MatchResult[T] = mr.when(!b, m)
    /** when the condition is true the result it taken as is, when it's false, take its negation */
    def iff(b: Boolean): MatchResult[T] = if (b) mr else mr.not

  }

}

object MatchResultLogicalCombinators extends MatchResultLogicalCombinators

private [specs2]
trait MatchResultExecution {

  /**
   * Get the value of a MatchResult expression which possibly throws a MatchResultException.
   * @return either Left(result) if an exception was thrown, or Right(result) if no exception was thrown
   */
  private[specs2]
  def executeEither[T](result: =>MatchResult[T]): Either[MatchResult[T], MatchResult[T]] = {
    val executed = trye(result)(identity)
    executed match {
      case Left(e: MatchResultException[_]) => Left(e.matchResult.asInstanceOf[MatchResult[T]])
      case Left(e)                          => throw e
      case Right(m)                         => Right(m)
    }
  }
  /**
   * Get the value of a MatchResult expression which possibly throws a MatchResultException.
   * @return either the result in Left or the result in right
   */
  private[specs2]
  def execute[T](result: =>MatchResult[T]): MatchResult[T] =
    executeEither(result).fold(m1 => m1, m1 => m1)
}
object MatchResultExecution extends MatchResultExecution
