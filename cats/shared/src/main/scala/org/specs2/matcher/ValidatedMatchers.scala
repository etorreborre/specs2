package org.specs2.matcher

import cats.data.Validated

/**
 * Matchers for the Validated datatype
 */
trait ValidatedMatchers {
  def beValid[T](t: ValueCheck[T]) = ValidValidatedCheckedMatcher(t)
  def beValid[T] = ValidValidatedMatcher[T]()

  def beInvalid[T](t: ValueCheck[T]) = InvalidValidatedCheckedMatcher(t)
  def beInvalid[T] = InvalidValidatedMatcher[T]()
}

object ValidatedMatchers extends ValidatedMatchers

case class ValidValidatedMatcher[T]() extends OptionLikeMatcher[({type l[a]=Validated[_, a]})#l, T, T]("Valid", _.toOption)
case class ValidValidatedCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[({type l[a]=Validated[_, a]})#l, T, T]("Valid", _.toEither.right.toOption, check)

case class InvalidValidatedMatcher[T]() extends OptionLikeMatcher[({type l[a]=Validated[a, _]})#l, T, T]("Invalid", _.toEither.left.toOption)
case class InvalidValidatedCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[({type l[a]=Validated[a, _]})#l, T, T]("Invalid", _.toEither.left.toOption, check)

