package org.specs2
package matcher

import org.specs2.execute._
import org.specs2.matcher.describe.Diffable
import org.specs2.text.Quote._

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

object ValueCheck {
  implicit def typedValueCheck[T : Diffable](expected: T): BeEqualTypedValueCheck[T] =
    new BeEqualTypedValueCheck[T](expected)

  def alwaysOk[T] = new ValueCheck[T] {
    def check:    T => Result = (t: T) => StandardResults.success
    def checkNot: T => Result = (t: T) => StandardResults.failure
  }

  def toOptionCheck[T](valueCheck: ValueCheck[T]): ValueCheck[Option[T]] = new ValueCheck[Option[T]] {
    def check = (t: Option[T]) =>
      t.map(valueCheck.check).getOrElse(Failure("Expected a value, got None"))

    def checkNot = (t: Option[T]) =>
      ResultLogicalCombinators.combineResult(check(t)).not
  }
}
/**
 * implicit conversions used to create ValueChecks
 */
trait ValueChecks extends ValueChecksBase {
  /** a partial function returning an object having an AsResult instance can check a value */
  implicit def partialfunctionIsValueCheck[T, R : AsResult](f: PartialFunction[T, R]): ValueCheck[T] = new ValueCheck[T] {
    def check    = (t: T) => {
      if (f.isDefinedAt(t)) functionResult(AsResult(f(t)), t)
      else                  Failure("undefined function for "+q(t))
    }
    def checkNot = (t: T) => Results.negate(check(t))
  }

  /** a check of type T can be downcasted implicitly to a check of type S >: T */
  implicit def downcastBeEqualTypedValueCheck[T, S >: T](check: BeEqualTypedValueCheck[T]): ValueCheck[S] = check.downcast[S]
}

trait ValueChecksBase extends ValueChecksLowImplicits {

  /** a Matcher[T] can check a value */
  implicit def matcherIsValueCheck[T](m: Matcher[T]): ValueCheck[T] = new ValueCheck[T] {
    def check    = (t: T) => AsResult(m(Expectable(t)))
    def checkNot = (t: T) => AsResult(m.not(Expectable(t)))
  }

  /** an expected value can be used to check another value */
  def valueIsTypedValueCheck[T](expected: T)(implicit di: Diffable[T]): BeEqualTypedValueCheck[T] =
    ValueCheck.typedValueCheck(expected)
}

trait ValueChecksLowImplicits {
  /** a function returning an object having an AsResult instance can check a value */
  implicit def functionIsValueCheck[T, R : AsResult](f: T => R): ValueCheck[T] = new ValueCheck[T] {
    def check    = (t: T) => functionResult(AsResult(f(t)), t)
    def checkNot = (t: T) => Results.negate(check(t))
  }

  private[matcher] def functionResult[T](result: Result, t: T) =
    if (Seq("true", "false").contains(result.message)) result.mapMessage(m => s"the function returns ${q(m)} on ${q(t)}")
    else result

}

object ValueChecks extends ValueChecks

/** ValueCheck for a typed expected value. It uses the BeTypedEqualTo matcher */
case class BeEqualTypedValueCheck[T : Diffable](expected: T) extends ValueCheck[T] {
  private lazy val matcher = new EqualityMatcher(expected)
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



