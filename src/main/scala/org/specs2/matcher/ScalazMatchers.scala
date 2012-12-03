package org.specs2
package matcher

import org.scalacheck.{ Arbitrary, Shrink, Prop }
import scalaz.{Semigroup, Monoid, Validation, Success, Failure, Scalaz, Equal}
import scalaz.syntax.monoid._
import MatchResultLogicalCombinators._
import execute.ResultLogicalCombinators._

/**
 * This trait provides matchers for some Scalaz (http://github.com/scalaz) types.
 */
trait ScalazMatchers extends ScalazBaseMatchers with ScalazBeHaveMatchers { outer: AnyMatchers => }

trait ScalazBaseMatchers extends ScalaCheckMatchers with Expectations with ValidationMatchers { outer: AnyMatchers =>

  implicit def semigroupProperty[T](implicit s: Semigroup[T]): SemigroupProperty[T] = new SemigroupProperty[T]()(s)
  class SemigroupProperty[T]()(implicit sg: Semigroup[T]) {
    def isAssociative(implicit a: Arbitrary[T], s: Shrink[T]) = outer.isAssociative
    def isSemigroup(implicit a: Arbitrary[T], s: Shrink[T])   = outer.isAssociative
  }

  implicit def monoidProperty[T](m: Monoid[T]): MonoidProperty[T] = new MonoidProperty[T]()(m)
  class MonoidProperty[T]()(implicit m: Monoid[T]) extends SemigroupProperty()(m) {
    def isMonoid(implicit a: Arbitrary[T], s: Shrink[T]) = outer.isMonoid
    def hasNeutralElement(implicit a: Arbitrary[T], s: Shrink[T]) = outer.hasZero
  }

  /** this ScalaCheck property is valid if the semigroup is associative */
  def isAssociative[T](implicit sg: Semigroup[T], a: Arbitrary[T], s: Shrink[T]): Prop =
    prop { (b1: T, b2: T, b3: T) => be_==(b1 |+| (b2 |+| b3)).apply(createExpectable((b1 |+| b2) |+| b3)) }

  /** this ScalaCheck property is valid if the monoid has a Zero element */
  def hasZero[T](implicit m: Monoid[T], a: Arbitrary[T], s: Shrink[T]): Prop =
    prop { (t: T) =>
      be_==(t |+| m.zero).apply(createExpectable(t)) and be_==(m.zero |+| t).apply(createExpectable(t))
    }

  /** this ScalaCheck property is valid if the monoid verifies the monoid laws */
  def isMonoid[T](implicit m: Monoid[T], a: Arbitrary[T], s: Shrink[T]) = isAssociative && hasZero

  /** equality matcher with an Equal typeclass */
  def equal[T : Equal](t: T): Matcher[T] = beTypedEqualTo(t, implicitly[Equal[T]].equal(_:T,_:T))

}

trait ScalazBeHaveMatchers { outer: ScalazMatchers with AnyMatchers =>
  implicit def scalazBeHaveMatcher[T : Equal](result: MatchResult[T]) = new ScalazBeHaveMatchers(result)
  class ScalazBeHaveMatchers[T : Equal](result: MatchResult[T]) {
    def equal(t: T) = result(outer.equal(t))
  }
}
