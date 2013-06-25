package org.specs2
package matcher

import text.NotNullStrings._
import text.Quote._
import org.specs2.execute.{Success, Failure, Result}

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

  def beRight[T] = new RightMatcher[T]
  class RightMatcher[T] extends Matcher[Either[_, T]] {
    def apply[S <: Either[_, T]](value: Expectable[S]) = {
      result(value.value.right.toOption.isDefined,
             value.description + " is Right",
             value.description + " is not Right",
             value)
    }

    def like(f: PartialFunction[T, MatchResult[_]]) = this and partialMatcher(f)

    private def partialMatcher(f: PartialFunction[T, MatchResult[_]]) = new Matcher[Either[_, T]] {
      def apply[S <: Either[_, T]](value: Expectable[S]) = {
        val res: Result = value.value match {
          case Right(t) if f.isDefinedAt(t)  => f(t).toResult.prependMessage("is Right")
          case Right(t) if !f.isDefinedAt(t) => Failure("is Right but the function is undefined at "+t.notNull)
          case other                         => Failure("is not Right")
        }
        result(res.isSuccess,
               value.description+" "+res.message,
               value.description+" "+res.message,
               value)
      }
    }
  }

  def right[T](t: =>T) = beRight(t)
  def right[T] = beRight

  def beLeft[T](t: =>T) = new Matcher[Either[T, _]] {
    def apply[S <: Either[T, _]](value: Expectable[S]) = {
      val expected = t
      result(value.value == Left(t),
             value.description + " is Left with value" + q(expected),
             value.description + " is not Left with value" + q(expected),
             value)
    }
  }

  def beLeft[T] = new LeftMatcher[T]
  class LeftMatcher[T] extends Matcher[Either[T, _]] {
    def apply[S <: Either[T, _]](value: Expectable[S]) = {
      result(value.value.left.toOption.isDefined,
             value.description + " is Left",
             value.description + " is not Left",
             value)
    }

    def like(f: PartialFunction[T, MatchResult[_]]) = this and partialMatcher(f)

    private def partialMatcher(f: PartialFunction[T, MatchResult[_]]) = new Matcher[Either[T, _]] {
      def apply[S <: Either[T, _]](value: Expectable[S]) = {
        val res: Result = value.value match {
          case Left(t) if f.isDefinedAt(t)  => f(t).toResult.prependMessage("is Left")
          case Left(t) if !f.isDefinedAt(t) => Failure("is Left but the function is undefined at "+t.notNull)
          case other                        => Failure("is not Left")
        }
        result(res.isSuccess,
               value.description+" "+res.message,
               value.description+" "+res.message,
               value)
      }
    }
  }

  def left[T](t: =>T) = beLeft(t)
  def left[T] = beLeft
}

private[specs2]
trait EitherBeHaveMatchers { outer: EitherBaseMatchers =>
  implicit def toEitherResultMatcher[L, R](result: MatchResult[Either[L, R]]) = new EitherResultMatcher(result)
  class EitherResultMatcher[L, R](result: MatchResult[Either[L, R]]) {
    def right(r: =>R) = result(outer.beRight(r))
    def left(l: =>L) = result(outer.beLeft(l))
    def beRight(r: =>R) = result(outer.beRight(r))
    def beLeft(l: =>L) = result(outer.beLeft(l))

    def right = result(outer.beRight)
    def left = result(outer.beLeft)
    def beRight = result(outer.beRight)
    def beLeft = result(outer.beLeft)
  }
}
