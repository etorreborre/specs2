package org.specs2
package matcher

import java.math._
import text.Plural._
import NumericMatchersDescription._
import math.Ordering.Implicits.infixOrderingOps

/**
 * Matchers for Numerical values
 */
trait NumericMatchers extends NumericBaseMatchers with NumericBeHaveMatchers:
  /** implicit definition to create delta for the beCloseTo matcher */
  implicit def ToDelta[S : Numeric](n: S): CanHaveDelta[S] = CanHaveDelta(n)

  /** implicit definition to create significant figures for the beCloseTo matcher */
  implicit class SignificantFiguresSyntax(value: Int):
    def significantFigures = SignificantFigures(value)
    def significantFigure = SignificantFigures(value)
  /** implicit definition to create significant figures for the beCloseTo matcher */
  implicit class SignificantSyntax[N : Numeric](target: N):
    def within(significant: SignificantFigures): SignificantTarget[N] =
    SignificantTarget(target, significant)



private[specs2]
trait NumericBaseMatchers:
  /** matches if actual <= n */
  def beLessThanOrEqualTo[S : Ordering](n: S) = new BeLessThanOrEqualTo(n)
  /** matches if actual <= n */
  def lessThanOrEqualTo[S : Ordering](n: S) = beLessThanOrEqualTo(n)
  /** alias for beLessThanOrEqualTo */
  def be_<=[S : Ordering](n: S) = beLessThanOrEqualTo(n)
  /** alias for beLessThanOrEqualTo */
  def <=[S : Ordering](n: S) = beLessThanOrEqualTo(n)
  /** matches if actual < n */
  def beLessThan[S : Ordering](n: S) = new BeLessThan(n)
  def lessThan[S : Ordering](n: S) = beLessThan(n)
  /** alias for beLessThan */
  def be_<[S : Ordering](n: S) = beLessThan(n)
  /** alias for beLessThan */
  def <[S : Ordering](n: S) = beLessThan(n)
  /** matches if actual >= n */
  def beGreaterThanOrEqualTo[S : Ordering](n: S) = new BeLessThan(n).not
  def greaterThanOrEqualTo[S : Ordering](n: S) = beGreaterThanOrEqualTo(n)
  /** alias for beGreaterThanOrEqualTo */
  def be_>=[S : Ordering](n: S) = beGreaterThanOrEqualTo(n)
  /** alias for beGreaterThanOrEqualTo */
  def >=[S : Ordering](n: S) = beGreaterThanOrEqualTo(n)
  /** matches if actual > n */
  def beGreaterThan[S : Ordering](n: S) = new BeLessThanOrEqualTo(n).not
  def greaterThan[S : Ordering](n: S) = beGreaterThan(n)
  /** alias for beGreaterThan */
  def be_>[S : Ordering](n: S) = beGreaterThan(n)
  /** alias for beGreaterThan */
  def >[S : Ordering](n: S) = beGreaterThan(n)

  /** matches if actual = n +/- delta */
  def beCloseTo[S : Numeric](n: S, delta: S): Matcher[S] = new BeCloseTo(n, delta)
  def closeTo[S : Numeric](n: S, delta: S): Matcher[S] = beCloseTo(n, delta)
  /** matches if actual = n +/- delta */
  def beCloseTo[S : Numeric](delta: PlusOrMinus[S]): Matcher[S] = beCloseTo(delta.n, delta.delta)
  def closeTo[S : Numeric](delta: PlusOrMinus[S]): Matcher[S] = beCloseTo(delta)
  /** alias for beCloseTo */
  def ~[S : Numeric](n: S)(delta: S): Matcher[S] = beCloseTo(n, delta)
  /** alias for beCloseTo */
  def ~[S : Numeric](delta: PlusOrMinus[S]): Matcher[S] = beCloseTo(delta)

  /** matches if target - actual < 10 pow (log actual - significantDigits) */
  def beCloseTo[S : Numeric](target: S, figures: SignificantFigures): Matcher[S] =
    new BeSignificantlyCloseTo[S](target, figures)

  def beCloseTo[S : Numeric](target: SignificantTarget[S]): Matcher[S] =
    new BeSignificantlyCloseTo[S](target.target, target.significantFigures)

  def closeTo[S : Numeric](target: S, figures: SignificantFigures): Matcher[S] =
    beCloseTo(target, figures)

  def closeTo[S : Numeric](target: SignificantTarget[S]): Matcher[S] =
    beCloseTo(target)


  /** matches if a value is between 2 others according to an Ordering */
  def beBetween[T : Ordering](t1: T, t2: T): BetweenMatcher[T] = BetweenMatcher(t1, t2)
  def between[T : Ordering](t1: T, t2: T): BetweenMatcher[T] = BetweenMatcher(t1, t2)

  /** alias for the adventurous: 5 must (`be[(2, 7)`[`) */
  def `be[`[T : Ordering](t1: T, t2: T): BetweenMatcher[T] = BetweenMatcher(t1, t2)
  /** alias for the adventurous: 5 must (`be](2, 7)`[`) */
  def `be]`[T : Ordering](t1: T, t2: T): BetweenMatcher[T] = BetweenMatcher(t1, t2).excludingStart

/** transient class allowing the creation of a delta */
case class CanHaveDelta[S : Numeric](n: S):
  def +/-(delta: S) = PlusOrMinus(n, delta)
/** class representing a numeric range */
case class PlusOrMinus[S](n: S, delta: S)

private[specs2]
trait NumericBeHaveMatchers extends BeHaveMatchers { outer: NumericBaseMatchers =>
  /**
   * matcher aliases and implicits to use with be + matcher
   */

  implicit def toOrderedResultMatcher[S : Ordering](result: MatchResult[S]): OrderedResultMatcher[S] = new OrderedResultMatcher(result)
  class OrderedResultMatcher[S : Ordering](result: MatchResult[S]):
    def be_<=(n: S) = result(outer.beLessThanOrEqualTo(n))
    def <=(n: S) = result(outer.beLessThanOrEqualTo(n))
    def lessThanOrEqualTo(n: S) = result(outer.beLessThanOrEqualTo(n))
    def beLessThanOrEqualTo(n: S) = result(outer.beLessThanOrEqualTo(n))
    def be_<(n: S) = result(outer.beLessThan(n))
    def <(n: S) = result(outer.beLessThan(n))
    def lessThan(n: S) = result(outer.beLessThan(n))
    def beLessThan(n: S) = result(outer.beLessThan(n))
    def be_>=(n: S) = result(outer.beGreaterThanOrEqualTo(n))
    def >=(n: S) = result(outer.beGreaterThanOrEqualTo(n))
    def beGreaterThanOrEqualTo(n: S) = result(outer.beGreaterThanOrEqualTo(n))
    def greaterThanOrEqualTo(n: S) = result(outer.beGreaterThanOrEqualTo(n))
    def be_>(n: S) = result(outer.beGreaterThan(n))
    def >(n: S) = result(outer.beGreaterThan(n))
    def greaterThan(n: S) = result(outer.beGreaterThan(n))
    def beGreaterThan(n: S) = result(outer.beGreaterThan(n))
    def beBetween(s1: S, s2: S) = result(outer.beBetween(s1, s2))
    def between(s1: S, s2: S) = result(outer.beBetween(s1, s2))

  implicit def toNumericResultMatcher[S : Numeric](result: MatchResult[S]): NumericResultMatcher[S] = new NumericResultMatcher(result)
  class NumericResultMatcher[S : Numeric](result: MatchResult[S]):
    def beCloseTo(n: S, delta: S) = result(outer.beCloseTo(n, delta))
    def beCloseTo(delta: PlusOrMinus[S]) = result(outer.beCloseTo(delta))
    def closeTo(n: S, delta: S) = result(outer.beCloseTo(n, delta))
    def closeTo(delta: PlusOrMinus[S]) = result(outer.beCloseTo(delta))
    def ~(n: S, delta: S) = result(outer.beCloseTo(n, delta))
    def ~(delta: PlusOrMinus[S]) = result(outer.beCloseTo(delta))
    def closeTo(target: S, figures: SignificantFigures) = result(outer.beCloseTo(target, figures))
    def closeTo(target: SignificantTarget[S]) = result(outer.beCloseTo(target))
    def beCloseTo(target: S, figures: SignificantFigures) = result(outer.beCloseTo(target, figures))
    def beCloseTo(target: SignificantTarget[S]) = result(outer.beCloseTo(target))
  implicit def toNeutralMatcherOrdered(result: NeutralMatcher[Any]) : NeutralMatcherOrdered =
    new NeutralMatcherOrdered(result)
  class NeutralMatcherOrdered(result: NeutralMatcher[Any]):
    def <=[S : Ordering](n: S)    = outer.beLessThanOrEqualTo(n)
    def <[S : Ordering](n: S)     = outer.beLessThan(n)
    def >=[S : Ordering](n: S)    = outer.beGreaterThanOrEqualTo(n)
    def >[S : Ordering](n: S)     = outer.beGreaterThan(n)

  implicit def toNeutralMatcherNumeric(result: NeutralMatcher[Any]) : NeutralMatcherNumeric =
    new NeutralMatcherNumeric(result)
  class NeutralMatcherNumeric(result: NeutralMatcher[Any]):
    def ~[S : Numeric](n: S, delta: S) = beCloseTo(n, delta)
    def ~[S : Numeric](delta: PlusOrMinus[S]) = beCloseTo(delta)
}

object NumericMatchers extends NumericMatchers:
  import text.NotNullStrings._

  private[specs2] def description[S](e: Expectable[S]) =
    e.desc match
      case Some(d) => d(e.value.notNull)
      case None    => e.value.notNull

object NumericMatchersDescription:
  import text.NotNullStrings._

  private[specs2] def description[S](e: Expectable[S]) =
    e.desc match
      case Some(d) => d(e.value.notNull)
      case None    => e.value.notNull

class BeLessThanOrEqualTo[T : Ordering](n: T) extends Matcher[T]:
  def apply[S <: T](a: Expectable[S]) =
    val value: T = a.value
    val r = value <= n
    val isEqual = value == n
    result(r,
           if isEqual then description(a) + " is equal to " + n.toString else description(a) + " is less than " + n.toString,
           description(a) + " is greater than " + n.toString,
           a)
class BeLessThan[T : Ordering](n: T) extends Matcher[T]:
  def apply[S <: T](a: Expectable[S]) =
    val value: T = a.value
    val r = value < n
    result(r,
           description(a) + " is less than " + n.toString,
           description(a) + " is not less than " + n.toString,
           a)
class BeCloseTo[T : Numeric](n: T, delta: T) extends Matcher[T]:
  def apply[S <: T](x: Expectable[S]) =
    val num = implicitly[Numeric[T]]
    result(num.lteq(num.minus(n, delta), x.value) && num.lteq(x.value, num.plus(n, delta)),
           description(x) + " is close to " + n.toString + " +/- " + delta,
           description(x) + " is not close to " + n.toString + " +/- " + delta, x)

class BeSignificantlyCloseTo[T : Numeric](target: T, sf: SignificantFigures) extends Matcher[T]:
  def apply[S <: T](x: Expectable[S]) =
    val num = implicitly[Numeric[T]]
    val actualUnscaled = BigDecimal.valueOf(num.toDouble(x.value))

    val newScale = sf.number - actualUnscaled.precision + actualUnscaled.scale
    val actual = actualUnscaled.setScale(newScale, RoundingMode.HALF_UP)
    val expected = BigDecimal.valueOf(num.toDouble(target)).setScale(newScale, RoundingMode.HALF_UP)

    result(actual == expected,
      s"${description(x)} is close to $target with ${sf.number.qty("significant digit")}",
      s"${description(x)} is not close to $target with ${sf.number.qty("significant digit")}", x)

case class SignificantTarget[T : Numeric](target: T, significantFigures: SignificantFigures)
case class SignificantFigures(number: Int)

case class BetweenMatcher[T : Ordering](t1: T, t2: T, includeStart: Boolean = true, includeEnd: Boolean = true) extends Matcher[T]:
  def apply[S <: T](s: Expectable[S]) =
    val value: T = s.value
    val included = (includeStart && (value >= t1) || !includeStart && (value > t1)) &&
                   (includeEnd   && (value <= t2) || !includeEnd   && (value < t2))

    def bracket(b: Boolean) = if b then "[" else "]"
    val (start, end) = (bracket(includeStart), bracket(!includeEnd))

    val (ok, ko) = (s.description+" is in "+start+t1+", "+t2+end,
      s.description+" is not in "+start+t1+", "+t2+end)
    result(included, ok, ko, s)

  def `]` = copy(includeEnd = true)
  def `[` = copy(includeEnd = false)

  def excludingStart  = copy(includeStart = false)
  def excludingEnd    = copy(includeEnd = false)
  def excludingBounds = copy(includeStart = false, includeEnd = false)
