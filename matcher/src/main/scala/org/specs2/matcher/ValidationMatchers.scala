package org.specs2
package matcher

import scalaz.Validation

/**
 * Matchers for the Validation datatype
 */
trait ValidationMatchers {

  def beSuccess[T](t: ValueCheck[T]) = SuccessValidationCheckedMatcher(t)
  def beSuccess[T] = SuccessValidationMatcher[T]()

  def beFailure[T](t: ValueCheck[T]) = FailureValidationCheckedMatcher(t)
  def beFailure[T] = FailureValidationMatcher[T]()

}

object ValidationMatchers extends ValidationMatchers

case class SuccessValidationMatcher[T]() extends OptionLikeMatcher[({type l[a]=Validation[_, a]})#l, T, T]("Success", _.toOption)
case class SuccessValidationCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[({type l[a]=Validation[_, a]})#l, T, T]("Success", _.toOption, check)

case class FailureValidationMatcher[T]() extends OptionLikeMatcher[({type l[a]=Validation[a, _]})#l, T, T]("Failure", _.toEither.left.toOption)
case class FailureValidationCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[({type l[a]=Validation[a, _]})#l, T, T]("Failure", _.toEither.left.toOption, check)
