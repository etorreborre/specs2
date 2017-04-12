package org.specs2
package matcher

import cats.data.Xor

/**
 * Matchers for the Xor datatype
 */
trait XorMatchers extends EitherMatchers with MatchersImplicits {

  def beXorRight[T](t: ValueCheck[T]) = XorRightDisjunctionCheckedMatcher(t)
  def beXorRight[T] = XorRightDisjunctionMatcher[T]()

  def beXorLeft[T](t: ValueCheck[T]) = XorLeftDisjunctionCheckedMatcher(t)
  def beXorLeft[T] = XorLeftDisjunctionMatcher[T]()

}

object XorMatchers extends XorMatchers

case class XorRightDisjunctionMatcher[T]() extends OptionLikeMatcher[({type l[a]= _ Xor a})#l, T, T]("Right", (_:Any Xor T).toEither.right.toOption)
case class XorRightDisjunctionCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[({type l[a]= _ Xor a})#l, T, T]("Right", (_:Any Xor T).toEither.right.toOption, check)

case class XorLeftDisjunctionMatcher[T]() extends OptionLikeMatcher[({type l[a]= a Xor _})#l, T, T]("Left", (_:T Xor Any).toEither.left.toOption)
case class XorLeftDisjunctionCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[({type l[a]=a Xor _})#l, T, T]("Left", (_: T Xor Any).toEither.left.toOption, check)

