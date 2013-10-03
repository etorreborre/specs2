package org.specs2
package matcher

import execute._
import MatchersImplicits._
/**
 * Common interface for checks of a value of type T:
 *
 *  - a expected single value of type T
 *  - a Matcher[T]
 *  - a function returning a type R having an AsResult instance
 */
trait ValueCheck[T] { outer =>
  def check:    T => Result
  def checkNot: T => Result

  def negate = new ValueCheck[T] {
    def check: T => Result = outer.checkNot
    def checkNot: T => Result = outer.check
  }
}

/**
 * implicit conversions used to create ValueChecks
 */
trait ValueChecks extends ValueChecksLowImplicits {
  /** a Matcher[T] can check a value */
  implicit def matcherIsContainCheck[T](m: Matcher[T]): ValueCheck[T] = new ValueCheck[T] {
    def check    = (t: T) => AsResult(m(Expectable(t)))
    def checkNot = (t: T) => AsResult(m.not(Expectable(t)))
  }

  /** a function returning an object having an AsResult instance can check a value */
  implicit def functionIsContainCheck[T, R : AsResult](f: T => R): ValueCheck[T] = new ValueCheck[T] {
    def check    = (t: T) => AsResult(f(t))
    def checkNot = (t: T) => Results.negate(AsResult(f(t)))
  }

  /** a check of type T can be downcasted implicitly to a check of type S >: T */
  implicit def downcastBeEqualTypedContainCheck[T, S >: T](check: BeEqualTypedValueCheck[T]): ValueCheck[S] = check.downcast[S]
}

/** Lower implicit conversions to create ValueChecks */
trait ValueChecksLowImplicits {
  /** an expected value can be used to check another value */
  implicit def valueIsTypedContainCheck[T](expected: T): BeEqualTypedValueCheck[T] = new BeEqualTypedValueCheck[T](expected)
}
object ValueChecks extends ValueChecks

/** ValueCheck for a typed expected value. It uses the BeTypedEqualTo matcher */
case class BeEqualTypedValueCheck[T](expected: T) extends ValueCheck[T] {
  private lazy val matcher = new BeTypedEqualTo(expected)
  def check    = (t: T) => AsResult(matcher(Expectable(t)))
  def checkNot = (t: T) => AsResult(matcher.not(Expectable(t)))

  def downcast[S] = new BeEqualValueCheck[S](expected)

}

/** ValueCheck for an untyped expected value. It uses the BeEqualTo matcher */
case class BeEqualValueCheck[T](expected: Any) extends ValueCheck[T] {
  private lazy val matcher = new BeEqualTo(expected)
  def check    = (t: T) => AsResult(matcher(Expectable(t)))
  def checkNot = (t: T) => AsResult(matcher.not(Expectable(t)))
  }



