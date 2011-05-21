package org.specs2
package matcher

import org.scalacheck.{ Arbitrary, Shrink, Prop }
import org.specs2.internal.scalaz._, Scalaz._

/**
 * This trait provides matchers for some Scalaz (http://code.google.com/p/scalaz) datatypes.
 */
trait ScalazMatchers extends ScalaCheckMatchers with Expectations { outer: AnyMatchers =>

  implicit def toSemigroupProperty[T](implicit s: Semigroup[T]): SemigroupProperty[T] = new SemigroupProperty[T]()(s)
  class SemigroupProperty[T]()(implicit sg: Semigroup[T]) {
    def isAssociative(implicit a: Arbitrary[T], s: Shrink[T]) = outer.isAssociative
  }

  def isAssociative[T](implicit sg: Semigroup[T], a: Arbitrary[T], s: Shrink[T]): Prop = {
    check { (b1: T, b2: T, b3: T) => be_==(b1 |+| (b2 |+| b3)).apply(createExpectable((b1 |+| b2) |+| b3)) }
  }

  implicit def toMonoidProperty[T](m: Monoid[T]): MonoidProperty[T] = new MonoidProperty[T]()(m)
  class MonoidProperty[T]()(implicit m: Monoid[T]) extends SemigroupProperty()(m) {
    def isMonoid(implicit a: Arbitrary[T], s: Shrink[T]) = outer.isMonoid
    def hasNeutralElement(implicit a: Arbitrary[T], s: Shrink[T]) = outer.hasNeutralElement
  }

  def hasNeutralElement[T](implicit m: Monoid[T], a: Arbitrary[T], s: Shrink[T]): Prop = {
    check { (t: T) =>
      be_==(t |+| m.zero).apply(createExpectable(t)) and be_==(m.zero |+| t).apply(createExpectable(t))
    }
  }

  def isMonoid[T](implicit m: Monoid[T], a: Arbitrary[T], s: Shrink[T]) = isAssociative and hasNeutralElement

}
