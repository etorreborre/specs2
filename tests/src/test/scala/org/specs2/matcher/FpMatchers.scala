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
trait FpMatchers extends ScalaCheck:
  private val outer = this

  extension [T](sg: Semigroup[T]):
    def isAssociative(using a: Arbitrary[T], s: Shrink[T]): Prop =
      outer.isAssociative(using sg, a, s)

    def isSemigroup(using a: Arbitrary[T], s: Shrink[T]): Prop =
      outer.isAssociative(using sg, a, s)

  extension [T](m: Monoid[T]):
    def isMonoid(using a: Arbitrary[T], s: Shrink[T]): Prop =
      outer.isMonoid(using m, a, s)

    def hasNeutralElement(using a: Arbitrary[T], s: Shrink[T]): Prop =
      outer.hasNeutralElement(using m, a, s)

  def isAssociative[T](using sg: Semigroup[T], a: Arbitrary[T], s: Shrink[T]): Prop =
    prop { (b1: T, b2: T, b3: T) =>
      be_==(b1 |+| (b2 |+| b3)).apply(createExpectable((b1 |+| b2) |+| b3))
    }.set(minTestsOk = 20, maxSize = 10)

  def hasNeutralElement[T](using m: Monoid[T], a: Arbitrary[T], s: Shrink[T]): Prop =
    prop { (t: T) =>
      be_==(t |+| m.zero).apply(createExpectable(t)) and be_==(m.zero |+| t).apply(createExpectable(t))
    }.set(minTestsOk = 20, maxSize = 10)

  def isMonoid[T](using m: Monoid[T], a: Arbitrary[T], s: Shrink[T]): Prop =
    isAssociative && hasNeutralElement
