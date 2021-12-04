package org.specs2
package matcher

import control.*
import describe.Diffable
import scala.annotation.*

/** Matchers for the Either datatype
  */
trait EitherMatchers:

  @targetName("beRightWithValueCheck")
  def beRight[T](t: ValueCheck[T]): RightCheckedMatcher[T] =
    RightCheckedMatcher(t)

  def beRight[T]: RightMatcher[T] =
    new RightMatcher[T]

  def right[T: Diffable](t: T): RightCheckedMatcher[T] =
    beRight(ValueChecks.valueIsTypedValueCheck(t))

  @targetName("rightWithValueCheck")
  def right[T](t: ValueCheck[T]): RightCheckedMatcher[T] =
    beRight(t)

  def right[T] = beRight

  @targetName("beLeftWithValueCheck")
  def beLeft[T](t: ValueCheck[T]): LeftCheckedMatcher[T] =
    LeftCheckedMatcher(t)

  def beLeft[T]: LeftMatcher[T] =
    LeftMatcher[T]()

  def left[T: Diffable](t: T): LeftCheckedMatcher[T] =
    beLeft(ValueChecks.valueIsTypedValueCheck(t))

  @targetName("leftWithValueCheck")
  def left[T](t: ValueCheck[T]): LeftCheckedMatcher[T] =
    beLeft(t)

  def left[T]: LeftMatcher[T] =
    beLeft

object EitherMatchers extends EitherMatchers

case class RightMatcher[T]() extends OptionLikeMatcher[Either[Any, T], T]("Right", (_: Either[Any, T]).toOption)
case class RightCheckedMatcher[T](check: ValueCheck[T])
    extends OptionLikeCheckedMatcher[Either[Any, T], T]("Right", (_: Either[Any, T]).toOption, check)

case class LeftMatcher[T]() extends OptionLikeMatcher[Either[T, Any], T]("Left", (_: Either[T, Any]).left.toOption)
case class LeftCheckedMatcher[T](check: ValueCheck[T])
    extends OptionLikeCheckedMatcher[Either[T, Any], T]("Left", (_: Either[T, Any]).left.toOption, check)
