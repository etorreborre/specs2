package org.specs2
package matcher

import execute._
import org.specs2.execute.AsResult
import describe._
import text.Quote._
import Expectations._
import control._
import ResultLogicalCombinators.{given _, _}

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

object ValueCheck:

  given typedValueCheck[T : Diffable] as Conversion[T, BeEqualTypedValueCheck[T]] {
    def apply(expected: T): BeEqualTypedValueCheck[T] =
      new BeEqualTypedValueCheck[T](expected)
  }

  def alwaysOk[T]: ValueCheck[T] =
    new ValueCheck[T]:
      def check:    T => Result = (t: T) => StandardResults.success
      def checkNot: T => Result = (t: T) => StandardResults.failure

  def toOptionCheck[T](valueCheck: ValueCheck[T]): ValueCheck[Option[T]] =
    new ValueCheck[Option[T]]:
      def check = (t: Option[T]) =>
        t.map(valueCheck.check).getOrElse(Failure("Expected a value, got None"))

      def checkNot = (t: Option[T]) =>
        check(t).not

/**
 * implicit conversions used to create ValueChecks
 */
trait ValueChecks extends ValueChecksBase:

  /** a partial function returning an object having an AsResult instance can check a value */
  given partialfunctionIsValueCheck[T, R : AsResult] as Conversion[PartialFunction[T, R], ValueCheck[T]] {
    def apply(f: PartialFunction[T, R]): ValueCheck[T] =
      new ValueCheck[T]:
       def check = (t: T) => {
         if f.isDefinedAt(t) then functionResult(AsResult.safely(f(t)), t)
         else                  Failure("undefined function for "+q(t))
       }

       def checkNot = (t: T) => Results.negate(check(t))
  }

  /** a check of type T can be downcasted implicitly to a check of type S >: T */
  given downcastBeEqualTypedValueCheck[T, S >: T] as Conversion[BeEqualTypedValueCheck[T], ValueCheck[S]] {
    def apply(check: BeEqualTypedValueCheck[T]): ValueCheck[S] =
      check.downcast[S]
  }

trait ValueChecksBase extends ValueChecksLowImplicits:

  /** a Matcher[T] can check a value */
  given matcherIsValueCheck[T] as Conversion[Matcher[T], ValueCheck[T]]{
    def apply(m: Matcher[T]): ValueCheck[T] =
      new ValueCheck[T]:
        def check    = (t: T) => AsResult.safely(m(createExpectable(t)))
        def checkNot = (t: T) => AsResult.safely(m.not(createExpectable(t)))
  }

  /** an expected value can be used to check another value */
  def valueIsTypedValueCheck[T](expected: T)(using di: Diffable[T]): BeEqualTypedValueCheck[T] =
    expected

trait ValueChecksLowImplicits:
  /** a function returning an object having an AsResult instance can check a value */
  given functionIsValueCheck[T, R : AsResult] as Conversion[T => R, ValueCheck[T]] {
    def apply(f: T => R): ValueCheck[T] =
      new ValueCheck[T]:
        def check    = (t: T) => functionResult(AsResult.safely(f(t)), t)
        def checkNot = (t: T) => Results.negate(check(t))
  }

  private[matcher] def functionResult[T](result: Result, t: T) =
    if Seq("true", "false").contains(result.message) then result.mapMessage(m => s"the function returns ${q(m)} on ${q(t)}")
    else result


object ValueChecks extends ValueChecks

/** ValueCheck for a typed expected value. It uses the BeTypedEqualTo matcher */
case class BeEqualTypedValueCheck[T : Diffable](expected: T) extends ValueCheck[T]:
  private lazy val matcher = new EqualityMatcher(expected)
  def check    = (t: T) => AsResult.safely(matcher(createExpectable(t)))
  def checkNot = (t: T) => AsResult.safely(matcher.not(createExpectable(t)))

  def downcast[S] = new BeEqualValueCheck[S](expected)


/** ValueCheck for an untyped expected value. It uses the BeEqualTo matcher */
case class BeEqualValueCheck[T](expected: Any) extends ValueCheck[T]:
  private lazy val matcher = new BeEqualTo(expected)
  def check    = (t: T) => AsResult.safely(matcher(createExpectable(t)))
  def checkNot = (t: T) => AsResult.safely(matcher.not(createExpectable(t)))
