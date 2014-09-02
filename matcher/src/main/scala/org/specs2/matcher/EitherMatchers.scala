package org.specs2
package matcher

/**
 * Matchers for the Either datatype
 */
trait EitherMatchers extends EitherBaseMatchers with EitherBeHaveMatchers
object EitherMatchers extends EitherMatchers

private[specs2]
trait EitherBaseMatchers {
  
  def beRight[T](t: ValueCheck[T]) = RightCheckedMatcher(t)
  def beRight[T] = new RightMatcher[T]

  def right[T](t: T) = beRight(t)
  def right[T](t: ValueCheck[T]) = beRight(t)
  def right[T] = beRight

  def beLeft[T](t: ValueCheck[T]) = LeftCheckedMatcher(t)
  def beLeft[T] = LeftMatcher[T]()

  def left[T](t: T) = beLeft(t)
  def left[T](t: ValueCheck[T]) = beLeft(t)
  def left[T] = beLeft
}

private[specs2]
trait EitherBeHaveMatchers extends BeHaveMatchers { outer: EitherBaseMatchers =>
  implicit class EitherResultMatcher[L, R](result: MatchResult[Either[L, R]]) {
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

case class RightMatcher[T]() extends OptionLikeMatcher[({type l[a]=Either[_, a]})#l, T, T]("Right", (_:Either[Any, T]).right.toOption)
case class RightCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[({type l[a]=Either[_, a]})#l, T, T]("Right", (_:Either[Any, T]).right.toOption, check)

case class LeftMatcher[T]() extends OptionLikeMatcher[({type l[a]=Either[a, _]})#l, T, T]("Left", (_:Either[T, Any]).left.toOption)
case class LeftCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[({type l[a]=Either[a, _]})#l, T, T]("Left", (_:Either[T, Any]).left.toOption, check)
