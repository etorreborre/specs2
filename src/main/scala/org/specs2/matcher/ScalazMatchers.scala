package org.specs2
package matcher

import org.scalacheck.{ Arbitrary, Shrink, Prop }
import org.specs2.internal._
import scalaz._, Scalaz._
import MatchResultLogicalCombinators._
import execute.{Result, ResultLogicalCombinators}
import ResultLogicalCombinators._

/**
 * This trait provides matchers for some Scalaz (http://code.google.com/p/scalaz) datatypes.
 *
 * It uses Scalaz classes from the repackaged version of scalaz 6 so it can not be used outside of specs2
 */
private[specs2]
trait ScalazMatchers extends ScalaCheckMatchers with Expectations { outer: AnyMatchers =>

  implicit def semigroupProperty[T](implicit s: Semigroup[T]): SemigroupProperty[T] = new SemigroupProperty[T]()(s)
  class SemigroupProperty[T]()(implicit sg: Semigroup[T]) {
    def isAssociative(implicit a: Arbitrary[T], s: Shrink[T]) = outer.isAssociative
    def isSemigroup(implicit a: Arbitrary[T], s: Shrink[T]) = outer.isAssociative
  }

  def isAssociative[T](implicit sg: Semigroup[T], a: Arbitrary[T], s: Shrink[T]): Prop =
    prop { (b1: T, b2: T, b3: T) => be_==(b1 |+| (b2 |+| b3)).apply(createExpectable((b1 |+| b2) |+| b3)) }

  implicit def monoidProperty[T](m: Monoid[T]): MonoidProperty[T] = new MonoidProperty[T]()(m)
  class MonoidProperty[T]()(implicit m: Monoid[T]) extends SemigroupProperty()(m) {
    def isMonoid(implicit a: Arbitrary[T], s: Shrink[T]) = outer.isMonoid
    def hasNeutralElement(implicit a: Arbitrary[T], s: Shrink[T]) = outer.hasNeutralElement
  }

  def hasNeutralElement[T](implicit m: Monoid[T], a: Arbitrary[T], s: Shrink[T]): Prop =
    prop { (t: T) =>
      be_==(t |+| m.zero).apply(createExpectable(t)) and be_==(m.zero |+| t).apply(createExpectable(t))
    }

  def isMonoid[T](implicit m: Monoid[T], a: Arbitrary[T], s: Shrink[T]) = isAssociative && hasNeutralElement

  import MatchersImplicits._

  /** success matcher for a Validation */
  def beSuccessful[E, A]: Matcher[Validation[E, A]] = (v: Validation[E, A]) => (v.fold(_ => false, _ => true), v+" successful", v+" is not successfull")

  /** failure matcher for a Validation */
  def beAFailure[E, A]: Matcher[Validation[E, A]] = (v: Validation[E, A]) => (v.fold(_ => true, _ => false), v+" is a failure", v+" is not a failure")

  /** success matcher for a Validation with a specific value */
  def succeedWith[E, A](a: =>A) = validationWith[E, A](Success(a))

  /** failure matcher for a Validation with a specific value */
  def failWith[E, A](e: =>E) = validationWith[E, A](Failure(e))

  private def validationWith[E, A](f: =>Validation[E, A]): Matcher[Validation[E, A]] = (v: Validation[E, A]) => {
    val expected = f
    (expected == v, v+" is a "+expected, v+" is not a "+expected)
  }

}
