package org.specs2
package matcher

import org.scalacheck.{ Arbitrary, Shrink, Prop }

import org.specs2.fp._
import org.specs2.fp.syntax._
import MatchResultLogicalCombinators._
import AnyMatchers._

/**
 * This trait provides matchers for some Scalaz (http://github/scalaz/scalaz) datatypes.
 */
private[specs2]
trait FpMatchers extends ScalaCheck { outer =>

  implicit def semigroupProperty[T](implicit s: Semigroup[T]): SemigroupProperty[T] = new SemigroupProperty[T]()(s)
  class SemigroupProperty[T]()(implicit sg: Semigroup[T]) {
    def isAssociative(implicit a: Arbitrary[T], s: Shrink[T]) = outer.isAssociative(sg, a, s)
    def isSemigroup(implicit a: Arbitrary[T], s: Shrink[T]) = outer.isAssociative(sg, a, s)
  }

  def isAssociative[T](implicit sg: Semigroup[T], a: Arbitrary[T], s: Shrink[T]): Prop =
    prop { (b1: T, b2: T, b3: T) => be_==(b1 |+| (b2 |+| b3)).apply(createExpectable((b1 |+| b2) |+| b3)) }

  implicit def monoidProperty[T](m: Monoid[T]): MonoidProperty[T] = new MonoidProperty[T]()(m)
  class MonoidProperty[T]()(implicit m: Monoid[T]) extends SemigroupProperty()(m) {
    def isMonoid(implicit a: Arbitrary[T], s: Shrink[T]) = outer.isMonoid(m, a, s)
    def hasNeutralElement(implicit a: Arbitrary[T], s: Shrink[T]) = outer.hasNeutralElement(m, a, s)
  }

  def hasNeutralElement[T](implicit m: Monoid[T], a: Arbitrary[T], s: Shrink[T]): Prop =
    prop { (t: T) =>
      be_==(t |+| m.zero).apply(createExpectable(t)) and be_==(m.zero |+| t).apply(createExpectable(t))
    }

  def isMonoid[T](implicit m: Monoid[T], a: Arbitrary[T], s: Shrink[T]) = isAssociative(m, a, s) && hasNeutralElement(m, a, s)

}
