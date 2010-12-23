package org.specs2
package matcher
import text.Quote._

/**
 * Matchers for Numerical values
 */
trait NumericMatchers extends NumericBaseMatchers with NumericBeHaveMatchers
object NumericMatchers extends NumericMatchers

private[specs2]
trait NumericBaseMatchers {
  /** matches if x <= n */   
  def beLessThanOrEqualTo[S <% Ordered[S]](n: S) = new BeLessThanOrEqualTo(n)
  def lessThanOrEqualTo[S <% Ordered[S]](n: S) = beLessThanOrEqualTo(n)
  /** @alias for beLessThanOrEqualTo */   
  def be_<=[S <% Ordered[S]](n: S) = beLessThanOrEqualTo(n)
  /** @alias for beLessThanOrEqualTo */   
  def <=[S <% Ordered[S]](n: S) = beLessThanOrEqualTo(n)
  /** matches if x < n */   
  def beLessThan[S <% Ordered[S]](n: S) = new BeLessThan(n)
  def lessThan[S <% Ordered[S]](n: S) = beLessThan(n)
  /** @alias for beLessThan */   
  def be_<[S <% Ordered[S]](n: S) = beLessThan(n)
  /** @alias for beLessThan */   
  def <[S <% Ordered[S]](n: S) = beLessThan(n)
  /** matches if x >= n */   
  def beGreaterThanOrEqualTo[S <% Ordered[S]](n: S) = new BeLessThan(n).not
  def greaterThanOrEqualTo[S <% Ordered[S]](n: S) = beGreaterThanOrEqualTo(n)
  /** @alias for beGreaterThanOrEqualTo */   
  def be_>=[S <% Ordered[S]](n: S) = beGreaterThanOrEqualTo(n)
  /** @alias for beGreaterThanOrEqualTo */   
  def >=[S <% Ordered[S]](n: S) = beGreaterThanOrEqualTo(n)
  /** matches if x > n */   
  def beGreaterThan[S <% Ordered[S]](n: S) = new BeLessThanOrEqualTo(n).not
  def greaterThan[S <% Ordered[S]](n: S) = beGreaterThan(n)
  /** @alias for beGreaterThan */   
  def be_>[S <% Ordered[S]](n: S) = beGreaterThan(n)
  /** @alias for beGreaterThan */   
  def >[S <% Ordered[S]](n: S) = beGreaterThan(n)
  
  /** implicit definition to create delta for the beCloseTo matcher */
  implicit def ToDelta[S : Numeric](n: S): CanHaveDelta[S] = CanHaveDelta(n)
  /** transient class allowing the creation of a delta */
  private[specs2]
  case class CanHaveDelta[S : Numeric](n: S) {
    def +/-(delta: S) = Delta(n, delta)
  }
  /** class representing a numeric range */
  private[specs2]
  case class Delta[S](n: S, delta: S)
  /** matches if x = n +/- delta */   
  def beCloseTo[S : Numeric](n: S, delta: S): Matcher[S] = new BeCloseTo(n, delta)
  def closeTo[S : Numeric](n: S, delta: S): Matcher[S] = beCloseTo(n, delta)
  /** matches if x = n +/- delta */   
  def beCloseTo[S : Numeric](delta: Delta[S]): Matcher[S] = beCloseTo(delta.n, delta.delta)
  def closeTo[S : Numeric](delta: Delta[S]): Matcher[S] = beCloseTo(delta)
  /** @alias for beCloseTo */   
  def ~[S : Numeric](n: S)(delta: S): Matcher[S] = beCloseTo(n, delta)
  /** @alias for beCloseTo */   
  def ~[S : Numeric](delta: Delta[S]): Matcher[S] = beCloseTo(delta)

}
private[specs2]
trait NumericBeHaveMatchers { outer: NumericBaseMatchers =>
  /** 
   * matcher aliases and implicits to use with be + matcher 
   */
  implicit def toOrderedResultMatcher[S <% Ordered[S]](result: MatchResult[S]) = new OrderedResultMatcher(result)
  class OrderedResultMatcher[S <% Ordered[S]](result: MatchResult[S]) {
    def <=(n: S) = result.apply(beLessThanOrEqualTo(n)) 
    def lessThanOrEqualTo(n: S) = result.apply(beLessThanOrEqualTo(n)) 
    def <(n: S) = result.apply(beLessThan(n)) 
    def lessThan(n: S) = result.apply(beLessThan(n)) 
    def >=(n: S) = result.apply(beGreaterThanOrEqualTo(n)) 
    def greaterThanOrEqualTo(n: S) = result.apply(beGreaterThanOrEqualTo(n)) 
    def >(n: S) = result.apply(beGreaterThan(n)) 
    def greaterThan(n: S) = result.apply(beGreaterThan(n)) 
  }
  implicit def toNumericResultMatcher[S : Numeric](result: MatchResult[S]) = new NumericResultMatcher(result)
  class NumericResultMatcher[S : Numeric](result: MatchResult[S]) {
    def closeTo(n: S, delta: S) = result.apply(outer.beCloseTo(n, delta))
    def closeTo(delta: Delta[S]) = result.apply(outer.beCloseTo(delta))
    def ~(n: S, delta: S) = result.apply(outer.beCloseTo(n, delta))
    def ~(delta: Delta[S]) = result.apply(outer.beCloseTo(delta))
  }
  implicit def toNeutralMatcherOrdered(result: NeutralMatcher[Any]) : NeutralMatcherOrdered = 
    new NeutralMatcherOrdered(result)
  class NeutralMatcherOrdered(result: NeutralMatcher[Any]) {
    def <=[S <% Ordered[S]](n: S) = beLessThanOrEqualTo(n) 
    def <[S <% Ordered[S]](n: S) = beLessThan(n) 
    def >=[S <% Ordered[S]](n: S) = beGreaterThanOrEqualTo(n) 
    def >[S <% Ordered[S]](n: S) = beGreaterThan(n) 
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
           if (isEqual) a.value.toString + " is equal to " + n.toString else a.value.toString + " is less than " + n.toString, 
           a.value.toString + " is greater than " + n.toString,
           a)
  }
}
class BeLessThan[T <% Ordered[T]](n: T) extends Matcher[T] { 
  def apply[S <: T](a: Expectable[S]) = {
    val r = a.value < n
    result(r, 
           a.value.toString + " is less than " + n.toString, 
           a.value.toString + " is not less than " + n.toString,
           a)
  }
}
class BeCloseTo[T : Numeric](n: T, delta: T) extends Matcher[T] {
  def apply[S <: T](x: Expectable[S]) = {
    val num = implicitly[Numeric[T]]
    result(num.lteq(num.minus(n, delta), x.value) && num.lteq(x.value, num.plus(n, delta)), 
           x.description + " is close to " + n.toString + " +/- " + delta, 
           x.description + " is not close to " + n.toString + " +/- " + delta, x)
  }
}