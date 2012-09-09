package org.specs2
package matcher

import control.Exceptions._
import execute.ResultLogicalCombinators

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
    lazy val m = mr
    lazy val result = MatchResultExecution.executeEither(m)

    /** @return the logical or of two results */
    def or[S >: T](other: =>MatchResult[S]): MatchResult[S] = result.fold(m1 => other, m1 => new OrMatch(m1, other).evaluate)
    /** @return the logical and of two results */
    def and[S >: T](other: =>MatchResult[S]): MatchResult[S] = result.fold(m1 => m, m1 => new AndMatch(m1, other).evaluate)
    /** apply the matcher and return the logical or of two results */
    def or(other: Matcher[T]): MatchResult[T] =
      result.fold(m1 => m1.expectable.applyMatcher(other),
                  m1 => combineMatchResult(m1).or(m1.expectable.applyMatcher(other)))
    /** apply the matcher and return the logical and of two results */
    def and(other: Matcher[T]): MatchResult[T] =
      result.fold(m1 => m,
                  m1 => combineMatchResult(m1).and(m1.expectable.applyMatcher(other)))
    /** @return the negation of this result */
    def not: MatchResult[T] = result.fold(m1 => m1.negate, m1 => m1.negate)
  }

}

object MatchResultLogicalCombinators extends MatchResultLogicalCombinators

private [specs2]
trait MatchResultExecution {

  /**
   * Get the value of a MatchResult expression which possibly throws a MatchResultException.
   * @return either Left(result) if an exception was thrown, or Right(result) if no exception was thrown
   */
  def executeEither[T](result: =>MatchResult[T]): Either[MatchResult[T], MatchResult[T]] = {
    val executed = trye(result)(identity)
    executed match {
      case Left(e: MatchResultException[_]) => Left(e.matchResult.asInstanceOf[MatchResult[T]])
      case Left(e)                          => throw e
      case Right(m)                         => Right(m)
    }
  }
}

object MatchResultExecution extends MatchResultExecution
