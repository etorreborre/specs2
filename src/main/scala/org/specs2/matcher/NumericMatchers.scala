package org.specs2
package matcher

import text.Quote._

/**
 * Matchers for Numerical values
 */
trait NumericMatchers extends NumericBaseMatchers with NumericBeHaveMatchers
object NumericMatchers extends NumericMatchers {
  import text.NotNullStrings._

  private[specs2] def description[S](e: Expectable[S]) = {
    e.desc match {
      case Some(d) => d(e.value.notNull)
      case None    => e.value.notNull
    }
  }
}
import NumericMatchers._

private[specs2]
trait NumericBaseMatchers {
  /** matches if x <= n */   
  def beLessThanOrEqualTo[S <% Ordered[S]](n: S) = new BeLessThanOrEqualTo(n)
  def lessThanOrEqualTo[S <% Ordered[S]](n: S) = beLessThanOrEqualTo(n)
  /** alias for beLessThanOrEqualTo */
  def be_<=[S <% Ordered[S]](n: S) = beLessThanOrEqualTo(n)
  /** alias for beLessThanOrEqualTo */
  def <=[S <% Ordered[S]](n: S) = beLessThanOrEqualTo(n)
  /** matches if x < n */   
  def beLessThan[S <% Ordered[S]](n: S) = new BeLessThan(n)
  def lessThan[S <% Ordered[S]](n: S) = beLessThan(n)
  /** alias for beLessThan */
  def be_<[S <% Ordered[S]](n: S) = beLessThan(n)
  /** alias for beLessThan */
  def <[S <% Ordered[S]](n: S) = beLessThan(n)
  /** matches if x >= n */   
  def beGreaterThanOrEqualTo[S <% Ordered[S]](n: S) = new BeLessThan(n).not
  def greaterThanOrEqualTo[S <% Ordered[S]](n: S) = beGreaterThanOrEqualTo(n)
  /** alias for beGreaterThanOrEqualTo */
  def be_>=[S <% Ordered[S]](n: S) = beGreaterThanOrEqualTo(n)
  /** alias for beGreaterThanOrEqualTo */
  def >=[S <% Ordered[S]](n: S) = beGreaterThanOrEqualTo(n)
  /** matches if x > n */   
  def beGreaterThan[S <% Ordered[S]](n: S) = new BeLessThanOrEqualTo(n).not
  def greaterThan[S <% Ordered[S]](n: S) = beGreaterThan(n)
  /** alias for beGreaterThan */
  def be_>[S <% Ordered[S]](n: S) = beGreaterThan(n)
  /** alias for beGreaterThan */
  def >[S <% Ordered[S]](n: S) = beGreaterThan(n)
  
  /** implicit definition to create delta for the beCloseTo matcher */
  implicit def ToDelta[S : Numeric](n: S): CanHaveDelta[S] = CanHaveDelta(n)
  /** matches if x = n +/- delta */
  def beCloseTo[S : Numeric](n: S, delta: S): Matcher[S] = new BeCloseTo(n, delta)
  def closeTo[S : Numeric](n: S, delta: S): Matcher[S] = beCloseTo(n, delta)
  /** matches if x = n +/- delta */   
  def beCloseTo[S : Numeric](delta: Delta[S]): Matcher[S] = beCloseTo(delta.n, delta.delta)
  def closeTo[S : Numeric](delta: Delta[S]): Matcher[S] = beCloseTo(delta)
  /** alias for beCloseTo */
  def ~[S : Numeric](n: S)(delta: S): Matcher[S] = beCloseTo(n, delta)
  /** alias for beCloseTo */
  def ~[S : Numeric](delta: Delta[S]): Matcher[S] = beCloseTo(delta)

  /** matches if a value is between 2 others according to an Ordering */
  def beBetween[T <% Ordered[T]](t1: T, t2: T): BetweenMatcher[T] = BetweenMatcher(t1, t2)
  def between[T <% Ordered[T]](t1: T, t2: T): BetweenMatcher[T] = BetweenMatcher(t1, t2)

  /** alias for the adventurous: 5 must (`be[(2, 7)`[`) */
  def `be[`[T <% Ordered[T]](t1: T, t2: T): BetweenMatcher[T] = BetweenMatcher(t1, t2)
  /** alias for the adventurous: 5 must (`be](2, 7)`[`) */
  def `be]`[T <% Ordered[T]](t1: T, t2: T): BetweenMatcher[T] = BetweenMatcher(t1, t2).excludingStart
}

/** transient class allowing the creation of a delta */
private[specs2]
case class CanHaveDelta[S : Numeric](n: S) {
  def +/-(delta: S) = Delta(n, delta)
}
/** class representing a numeric range */
case class Delta[S](n: S, delta: S)

private[specs2]
trait NumericBeHaveMatchers { outer: NumericBaseMatchers =>
  /** 
   * matcher aliases and implicits to use with be + matcher
   */
  implicit def toOrderedResultMatcher[S <% Ordered[S]](result: MatchResult[S]) = new OrderedResultMatcher(result)
  class OrderedResultMatcher[S <% Ordered[S]](result: MatchResult[S]) {
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
  }
  implicit def toNumericResultMatcher[S : Numeric](result: MatchResult[S]) = new NumericResultMatcher(result)
  class NumericResultMatcher[S : Numeric](result: MatchResult[S]) {
    def beCloseTo(n: S, delta: S) = result(outer.beCloseTo(n, delta))
    def beCloseTo(delta: Delta[S]) = result(outer.beCloseTo(delta))
    def closeTo(n: S, delta: S) = result(outer.beCloseTo(n, delta))
    def closeTo(delta: Delta[S]) = result(outer.beCloseTo(delta))
    def ~(n: S, delta: S) = result(outer.beCloseTo(n, delta))
    def ~(delta: Delta[S]) = result(outer.beCloseTo(delta))
  }
  implicit def toNeutralMatcherOrdered(result: NeutralMatcher[Any]) : NeutralMatcherOrdered = 
    new NeutralMatcherOrdered(result)
  class NeutralMatcherOrdered(result: NeutralMatcher[Any]) {
    def <=[S <% Ordered[S]](n: S)    = outer.beLessThanOrEqualTo(n)
    def <[S <% Ordered[S]](n: S)     = outer.beLessThan(n)
    def >=[S <% Ordered[S]](n: S)    = outer.beGreaterThanOrEqualTo(n)
    def >[S <% Ordered[S]](n: S)     = outer.beGreaterThan(n)
  }

  implicit def toNeutralMatcherNumeric(result: NeutralMatcher[Any]) : NeutralMatcherNumeric =
    new NeutralMatcherNumeric(result)
  class NeutralMatcherNumeric(result: NeutralMatcher[Any]) {
    def ~[S : Numeric](n: S, delta: S) = beCloseTo(n, delta)
    def ~[S : Numeric](delta: Delta[S]) = beCloseTo(delta)
  }
}

class BeLessThanOrEqualTo[T <% Ordered[T]](n: T) extends Matcher[T] { 
  def apply[S <: T](a: Expectable[S]) = {
    val r = a.value <= n
    val isEqual = a.value == n  
    result(r, 
           if (isEqual) description(a) + " is equal to " + n.toString else description(a) + " is less than " + n.toString,
           description(a) + " is greater than " + n.toString,
           a)
  }
}
class BeLessThan[T <% Ordered[T]](n: T) extends Matcher[T] { 
  def apply[S <: T](a: Expectable[S]) = {
    val r = a.value < n
    result(r, 
           description(a) + " is less than " + n.toString,
           description(a) + " is not less than " + n.toString,
           a)
  }
}
class BeCloseTo[T : Numeric](n: T, delta: T) extends Matcher[T] {
  def apply[S <: T](x: Expectable[S]) = {
    val num = implicitly[Numeric[T]]
    result(num.lteq(num.minus(n, delta), x.value) && num.lteq(x.value, num.plus(n, delta)), 
           description(x) + " is close to " + n.toString + " +/- " + delta,
           description(x) + " is not close to " + n.toString + " +/- " + delta, x)
  }
}

case class BetweenMatcher[T <% Ordered[T]](t1: T, t2: T, includeStart: Boolean = true, includeEnd: Boolean = true) extends Matcher[T] {
  def apply[S <: T](s: Expectable[S]) = {
    val value = s.value
    val included = (includeStart && (value >= t1) || !includeStart && (value > t1)) &&
                   (includeEnd   && (value <= t2) || !includeEnd   && (value < t2))

    def bracket(b: Boolean) = if (b) "[" else "]"
    val (start, end) = (bracket(includeStart), bracket(!includeEnd))

    val (ok, ko) = (s.value+" is in "+start+t1+", "+t2+end,
      s.value+" is not in "+start+t1+", "+t2+end)
    result(included, ok, ko, s)
  }

  def `]` = copy(includeEnd = true)
  def `[` = copy(includeEnd = false)

  def excludingStart  = copy(includeStart = false)
  def excludingEnd    = copy(includeEnd = false)
  def excludingBounds = copy(includeStart = false, includeEnd = false)
}
