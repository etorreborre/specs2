package org.specs2
package matcher

import scalaz.\/

/**
 * Matchers for the \/ datatype
 */
trait DisjunctionMatchers {

  def be_\/-[T](t: ValueCheck[T]) = RightDisjunctionCheckedMatcher(t)
  def be_\/-[T] = RightDisjunctionMatcher[T]()

  def be_-\/[T](t: ValueCheck[T]) = LeftDisjunctionCheckedMatcher(t)
  def be_-\/[T] = LeftDisjunctionMatcher[T]()

}

object DisjunctionMatchers extends DisjunctionMatchers

case class RightDisjunctionMatcher[T]() extends OptionLikeMatcher[({type l[a]= _ \/ a})#l, T, T]("\\/-", (_:Any \/ T).toEither.right.toOption)
case class RightDisjunctionCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[({type l[a]= _ \/ a})#l, T, T]("\\/-", (_:Any \/ T).toEither.right.toOption, check)

case class LeftDisjunctionMatcher[T]() extends OptionLikeMatcher[({type l[a]= a \/ _})#l, T, T]("-\\/", (_:T \/ Any).toEither.left.toOption)
case class LeftDisjunctionCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[({type l[a]=a \/ _})#l, T, T]("-\\/", (_: T \/Any).toEither.left.toOption, check)
