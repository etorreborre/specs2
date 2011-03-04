package org.specs2
package matcher

import org.scalacheck.{ Arbitrary, Shrink }
import scalaz._, Scalaz._

/**
 * This trait provides matchers for some Scalaz (http://code.google.com/p/scalaz) datatypes.
 */
trait ScalazMatchers extends ScalaCheckMatchers with Expectations { outer: AnyMatchers =>

  implicit def toSemigroupProperty[T](s: Semigroup[T]): SemigroupProperty[T] = new SemigroupProperty(s)
  class SemigroupProperty[T](sg: Semigroup[T]) {
    def isAssociative(implicit a: Arbitrary[T], s: Shrink[T], p: Parameters) = 
      outer.isAssociative(sg, a, s, p)
  }

  def isAssociative[T](implicit sg: Semigroup[T], a: Arbitrary[T], s: Shrink[T], p: Parameters) = {
    check3 { (b1: T, b2: T, b3: T) =>
    be_==(b1 |+| (b2 |+| b3)).apply(createExpectable((b1 |+| b2) |+| b3)) }(a, s, a, s, a, s, p)
  }

  implicit def toMonoidProperty[T](m: Monoid[T]): MonoidProperty[T] = new MonoidProperty(m)
  class MonoidProperty[T](m: Monoid[T]) extends SemigroupProperty(m) {
    def isMonoid(implicit a: Arbitrary[T], s: Shrink[T], p: Parameters) = outer.isMonoid(m, a, s, p)
    def hasNeutralElement(implicit a: Arbitrary[T], s: Shrink[T], p: Parameters) = 
      outer.hasNeutralElement(m, a, s, p)
  }

  def hasNeutralElement[T](implicit m: Monoid[T], a: Arbitrary[T], s: Shrink[T], p: Parameters) = {
    check { (t: T) => 
      be_==(t |+| m.zero).apply(createExpectable(t)) and
      be_==(m.zero |+| t).apply(createExpectable(t))
    }(a, s, p)
  }

  def isMonoid[T](implicit m: Monoid[T], a: Arbitrary[T], s: Shrink[T], p: Parameters) = {
    isAssociative(m, a, s, p) and hasNeutralElement(m, a, s, p)
  }

}