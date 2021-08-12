package org.specs2
package matcher

import language.adhocExtensions
import control.*
import ImplicitParameters.{given, *}
import describe.Diffable

/** Matchers for the Either datatype
  */
trait EitherMatchers:

  def beRight[T](t: ValueCheck[T]) = RightCheckedMatcher(t)
  def beRight[T](using p: ImplicitParam = implicitParameter) = use(p)(new RightMatcher[T])

  def right[T: Diffable](t: T) = beRight(ValueChecks.valueIsTypedValueCheck(t))
  def right[T](t: ValueCheck[T]) = beRight(t)
  def right[T](using p: ImplicitParam = implicitParameter) = beRight(p)

  def beLeft[T](t: ValueCheck[T]): LeftCheckedMatcher[T] = LeftCheckedMatcher(t)
  def beLeft[T](using p: ImplicitParam = implicitParameter): LeftMatcher[T] = use(p)(LeftMatcher[T]())

  def left[T: Diffable](t: T) = beLeft(ValueChecks.valueIsTypedValueCheck(t))
  def left[T](t: ValueCheck[T]) = beLeft(t)
  def left[T](using p: ImplicitParam = implicitParameter) = beLeft(p)

object EitherMatchers extends EitherMatchers

case class RightMatcher[T]() extends OptionLikeMatcher[Either[Any, T], T]("Right", (_: Either[Any, T]).toOption)
case class RightCheckedMatcher[T](check: ValueCheck[T])
    extends OptionLikeCheckedMatcher[Either[Any, T], T]("Right", (_: Either[Any, T]).toOption, check)

case class LeftMatcher[T]() extends OptionLikeMatcher[Either[T, Any], T]("Left", (_: Either[T, Any]).left.toOption)
case class LeftCheckedMatcher[T](check: ValueCheck[T])
    extends OptionLikeCheckedMatcher[Either[T, Any], T]("Left", (_: Either[T, Any]).left.toOption, check)
