package org.specs2
package matcher

import java.math.*
import text.Plural.*
import NumericMatchersDescription.*
import math.Ordering.Implicits.infixOrderingOps
import execute.*, Result.*

/** Matchers for Numerical values
  */
trait NumericMatchers:
  /** implicit definition to create delta for the beCloseTo matcher */
  given [S: Numeric]: Conversion[S, CanHaveDelta[S]] with
    def apply(n: S): CanHaveDelta[S] =
      CanHaveDelta(n)

  /** implicit definition to create significant figures for the beCloseTo matcher */
  extension (value: Int)
    def significantFigures = SignificantFigures(value)
    def significantFigure = SignificantFigures(value)

  /** implicit definition to create significant figures for the beCloseTo matcher */
  extension [N: Numeric](target: N)
    def within(significant: SignificantFigures): SignificantTarget[N] =
      SignificantTarget(target, significant)

  /** matches if actual <= n */
  def beLessThanOrEqualTo[S: Ordering](n: S): BeLessThanOrEqualTo[S] =
    new BeLessThanOrEqualTo(n)

  /** matches if actual <= n */
  def lessThanOrEqualTo[S: Ordering](n: S): BeLessThanOrEqualTo[S] =
    beLessThanOrEqualTo(n)

  /** alias for beLessThanOrEqualTo */
  def be_<=[S: Ordering](n: S) = beLessThanOrEqualTo(n)

  /** alias for beLessThanOrEqualTo */
  def <=[S: Ordering](n: S) = beLessThanOrEqualTo(n)

  /** matches if actual < n */
  def beLessThan[S: Ordering](n: S) = new BeLessThan(n)
  def lessThan[S: Ordering](n: S) = beLessThan(n)

  /** alias for beLessThan */
  def be_<[S: Ordering](n: S) = beLessThan(n)

  /** alias for beLessThan */
  def <[S: Ordering](n: S) = beLessThan(n)

  /** matches if actual >= n */
  def beGreaterThanOrEqualTo[S: Ordering](n: S): Matcher[S] = new BeGreaterThanOrEqualTo(n)
  def greaterThanOrEqualTo[S: Ordering](n: S) = beGreaterThanOrEqualTo(n)

  /** alias for beGreaterThanOrEqualTo */
  def be_>=[S: Ordering](n: S) = beGreaterThanOrEqualTo(n)

  /** alias for beGreaterThanOrEqualTo */
  def >=[S: Ordering](n: S) = beGreaterThanOrEqualTo(n)

  /** matches if actual > n */
  def beGreaterThan[S: Ordering](n: S) = new BeGreaterThan(n)
  def greaterThan[S: Ordering](n: S) = beGreaterThan(n)

  /** alias for beGreaterThan */
  def be_>[S: Ordering](n: S) = beGreaterThan(n)

  /** alias for beGreaterThan */
  def >[S: Ordering](n: S) = beGreaterThan(n)

  /** matches if actual = n +/- delta */
  def beCloseTo[S: Numeric](n: S, delta: S): Matcher[S] = new BeCloseTo(n, delta)
  def closeTo[S: Numeric](n: S, delta: S): Matcher[S] = beCloseTo(n, delta)

  /** matches if actual = n +/- delta */
  def beCloseTo[S: Numeric](delta: PlusOrMinus[S]): Matcher[S] = beCloseTo(delta.n, delta.delta)
  def closeTo[S: Numeric](delta: PlusOrMinus[S]): Matcher[S] = beCloseTo(delta)

  /** alias for beCloseTo */
  def ~[S: Numeric](n: S)(delta: S): Matcher[S] = beCloseTo(n, delta)

  /** alias for beCloseTo */
  def be_~[S: Numeric](n: S)(delta: S): Matcher[S] = beCloseTo(n, delta)

  /** alias for beCloseTo */
  def ~[S: Numeric](delta: PlusOrMinus[S]): Matcher[S] = beCloseTo(delta)

  /** alias for beCloseTo */
  def be_~[S: Numeric](delta: PlusOrMinus[S]): Matcher[S] = beCloseTo(delta)

  /** matches if target - actual < 10 pow (log actual - significantDigits) */
  def beCloseTo[S: Numeric](target: S, figures: SignificantFigures): Matcher[S] =
    new BeSignificantlyCloseTo[S](target, figures)

  def beCloseTo[S: Numeric](target: SignificantTarget[S]): Matcher[S] =
    new BeSignificantlyCloseTo[S](target.target, target.significantFigures)

  def closeTo[S: Numeric](target: S, figures: SignificantFigures): Matcher[S] =
    beCloseTo(target, figures)

  def closeTo[S: Numeric](target: SignificantTarget[S]): Matcher[S] =
    beCloseTo(target)

  /** matches if a value is between 2 others according to an Ordering */
  def beBetween[T: Ordering](t1: T, t2: T): BetweenMatcher[T] = BetweenMatcher(t1, t2)
  def between[T: Ordering](t1: T, t2: T): BetweenMatcher[T] = BetweenMatcher(t1, t2)

object NumericMatchers extends NumericMatchers

/** transient class allowing the creation of a delta */
case class CanHaveDelta[S: Numeric](n: S):
  def +/-(delta: S) = PlusOrMinus(n, delta)

/** class representing a numeric range */
case class PlusOrMinus[S](n: S, delta: S)

object NumericMatchersDescription

class BeLessThanOrEqualTo[T: Ordering](n: T) extends Matcher[T]:
  def apply[S <: T](a: Expectable[S]) =
    val value: T = a.value
    val r = value <= n
    val isEqual = value == n
    result(r, a.description + " is strictly greater than " + n.toString)

class BeLessThan[T: Ordering](n: T) extends Matcher[T]:
  def apply[S <: T](a: Expectable[S]) =
    val value: T = a.value
    val r = value < n
    result(r, a.description + " is greater than " + n.toString)

class BeGreaterThanOrEqualTo[T: Ordering](n: T) extends Matcher[T]:
  def apply[S <: T](a: Expectable[S]) =
    val value: T = a.value
    val r = value >= n
    val isEqual = value == n
    result(r, a.description + " is strictly less than " + n.toString)

class BeGreaterThan[T: Ordering](n: T) extends Matcher[T]:
  def apply[S <: T](a: Expectable[S]) =
    val value: T = a.value
    val r = value > n
    result(r, a.description + " is less than " + n.toString)

class BeCloseTo[T: Numeric](n: T, delta: T) extends Matcher[T]:
  def apply[S <: T](x: Expectable[S]) =
    val num = implicitly[Numeric[T]]
    result(
      num.lteq(num.minus(n, delta), x.value) && num.lteq(x.value, num.plus(n, delta)),
      x.description + " is not close to " + n.toString + " +/- " + delta
    )

class BeSignificantlyCloseTo[T: Numeric](target: T, sf: SignificantFigures) extends Matcher[T]:
  def apply[S <: T](x: Expectable[S]) =
    val num = implicitly[Numeric[T]]
    val actualUnscaled = BigDecimal.valueOf(num.toDouble(x.value))

    val newScale = sf.number - actualUnscaled.precision + actualUnscaled.scale
    val actual = actualUnscaled.setScale(newScale, RoundingMode.HALF_UP)
    val expected = BigDecimal.valueOf(num.toDouble(target)).setScale(newScale, RoundingMode.HALF_UP)

    result(actual == expected, s"${x.description} is not close to $target with ${sf.number.qty("significant digit")}")

case class SignificantTarget[T: Numeric](target: T, significantFigures: SignificantFigures)
case class SignificantFigures(number: Int)

case class BetweenMatcher[T: Ordering](t1: T, t2: T, includeStart: Boolean = true, includeEnd: Boolean = true)
    extends Matcher[T]:
  def apply[S <: T](s: Expectable[S]) =
    val value: T = s.value
    val included = (includeStart && (value >= t1) || !includeStart && (value > t1)) &&
      (includeEnd && (value <= t2) || !includeEnd && (value < t2))

    def bracket(b: Boolean) = if b then "[" else "]"
    val (start, end) = (bracket(includeStart), bracket(!includeEnd))

    val (ok, ko) = (
      s.description + " is in " + start + t1 + ", " + t2 + end,
      s.description + " is not in " + start + t1 + ", " + t2 + end
    )
    result(included, ko)

  def excludingStart = copy(includeStart = false)
  def excludingEnd = copy(includeEnd = false)
  def excludingBounds = copy(includeStart = false, includeEnd = false)
