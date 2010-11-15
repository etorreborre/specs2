package org.specs2
package matcher

import org.scalacheck.{ Arbitrary, Shrink, Gen, Prop }
import scalaz._
import Scalaz._

/**
 * Matchers for Semigroups
 *
 */
trait SemigroupMatchers { outer: ScalaCheckMatchers with AnyMatchers =>
  
  implicit def toSemigroupProperty[T](s: Semigroup[T]): SemiGroupProperty[T] = new SemiGroupProperty(s)
  class SemiGroupProperty[T](sg: Semigroup[T]) {
    def isAssociative(implicit a: Arbitrary[T], s: Shrink[T]) = outer.isAssociative(sg, a, s)
  }

  def isAssociative[T](implicit sg: Semigroup[T], a: Arbitrary[T], s: Shrink[T]) = {
    check { (b1: T, b2: T, b3: T) => 
    be_==(b1 |+| (b2 |+| b3)).apply(Expectable((b1 |+| b2) |+| b3)) }
  }
}