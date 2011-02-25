package org.specs2
package matcher
import text.Quote._

/**
 * Matchers for the Either datatype
 */
trait EitherMatchers extends EitherBaseMatchers with EitherBeHaveMatchers
object EitherMatchers extends EitherMatchers

private[specs2]
trait EitherBaseMatchers {
  
  def beRight[T](t: =>T) = new Matcher[Either[_, T]] {
    def apply[S <: Either[_, T]](value: Expectable[S]) = {
      val expected = t
      result(value.value == Right(t), 
             value.description + " is Right with value" + q(expected),
             value.description + " is not Right with value" + q(expected),
             value)
    }
  }

  def right[T](t: =>T) = beRight(t)
  def beLeft[T](t: =>T) = new Matcher[Either[T, _]] {
    def apply[S <: Either[T, _]](value: Expectable[S]) = {
      val expected = t
      result(value.value == Left(t), 
             value.description + " is Left with value" + q(expected),
             value.description + " is not Left with value" + q(expected),
             value)
    }
  }

  def left[T](t: =>T) = beLeft(t)
}

private[specs2]
trait EitherBeHaveMatchers { outer: EitherBaseMatchers =>
  implicit def toEitherResultMatcher[L, R](result: MatchResult[Either[L, R]]) = new EitherResultMatcher(result)
  class EitherResultMatcher[L, R](result: MatchResult[Either[L, R]]) {
    def right(r: =>R) = result(outer.beRight(r))
    def left(l: =>L) = result(outer.beLeft(l))
  }
}
