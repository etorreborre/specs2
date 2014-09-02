package org.specs2
package matcher

import control.Exceptions._
import org.specs2.execute.{Result, ResultLogicalCombinators}
/**
 * This trait provides logical operators to combine match results where potentially a MatchResult expression
 * throws an Exception, either because it is an error or because it is a ThrownExpectation
 */
trait MatchResultCombinators extends MatchResultLogicalCombinators with ResultLogicalCombinators
object MatchResultCombinators extends MatchResultCombinators

trait MatchResultLogicalCombinators {

  implicit def combineMatchResult[T](m: =>MatchResult[T]): MatchResultCombinator[T] = new MatchResultCombinator[T](m)

  class MatchResultCombinator[T](mr: =>MatchResult[T]) {
    /**
     * Either  Left(exception) => in case of an error
     *     or  Right(result)   => normal result
     */
    lazy val result: Either[Exception, MatchResult[T]] =
      try Right(mr)
      catch {
        case failure: MatchResultException[_] => Right[Exception, MatchResult[T]](failure.matchResult.asInstanceOf[MatchResult[T]])
        case other: Exception                 => Left[Exception, MatchResult[T]](other)
      }

    /** if there was an exception on evaluating the result, no expectable can be accessed */
    lazy val expectable = 
      result.fold(e => throw e, _.expectable)

    /** @return the logical or of two results */
    def or[S >: T](other: =>MatchResult[S]): MatchResult[S] =
      result.fold(
        _ => other,   // error, evaluate the other result
        m1 => expectable.check(new OrMatch(m1, other).evaluate)) // otherwise use the OrMatch rules to evaluate m1 or other

    /** @return the logical or of a MatchResult and a Result */
    def or(other: =>Result): Result =
      result.fold(
        _ => other, // error, evaluate the other result 
        m1 => expectable.checkResult(ResultLogicalCombinators.combineResult(m1.toResult) or other)) // otherwise combine both results

    /** @return the logical and of two results */
    def and[S >: T](other: =>MatchResult[S]): MatchResult[S] =
      result.fold(
        e => throw e, // error, rethrow it
        m1 => expectable.check(new AndMatch(m1, other).evaluate)) // otherwise use the AndMatch rules to evaluate m1 and other

    /** @return the logical and of a MatchResult and a Result */
    def and(other: =>Result): Result =
      result.fold(
        e => throw e, // error, rethrow it
        // otherwise, use the result combinators
        m1 => expectable.checkResult(ResultLogicalCombinators.combineResult(m1.toResult) and other))

    /** apply the matcher and return the logical or of two results */
    def or(other: Matcher[T]): MatchResult[T] =
      tryOr {
        result.fold(
          e => throw e,
          m1 => combineMatchResult(m1).or(expectable.applyMatcher(other)))
      } { e => Expectable({ throw e; expectable.value }).applyMatcher(other) }

    /** apply the matcher and return the logical and of two results */
    def and(other: Matcher[T]): MatchResult[T] =
      result.fold(
        e => throw e, // error, rethrow it
        // otherwise apply the other matcher and use the AndMatch rules to evaluate m1 and the other result
        m1 => expectable.check(new AndMatch(m1, expectable.applyMatcher(other)).evaluate))

    /** @return the negation of this result */
    def not: MatchResult[T] = 
      result.fold(
        e => throw e, // error, rethrow it
        m1 => expectable.check(m1.negate))

    /** only consider this result if the condition is true */
    def when(condition: Boolean, m: String= ""): MatchResult[T] = if (condition) mr else MatchSuccess(m, m, expectable)
    /** only consider this result if the condition is false */
    def unless(condition: Boolean, m: String= ""): MatchResult[T] = mr.when(!condition, m)
    /** when the condition is true the result it taken as is, when it's false, take its negation */
    def iff(condition: Boolean): MatchResult[T] = if (condition) mr else mr.not

  }

}

object MatchResultLogicalCombinators extends MatchResultLogicalCombinators
