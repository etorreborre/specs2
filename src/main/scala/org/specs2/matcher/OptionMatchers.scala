package org.specs2
package matcher
import text.Quote._

/**
 * Matchers for Options
 */
trait OptionMatchers extends OptionBaseMatchers with OptionBeHaveMatchers
object OptionMatchers extends OptionMatchers

private[specs2]
trait OptionBaseMatchers {
  
  def beSome[T](t: =>T) = new Matcher[Option[T]] {
    def apply[S <: Option[T]](value: Expectable[S]) = {
      val expected = t
      result(value.value == Some(t), 
             value.description + " is Some with value" + q(expected),
             value.description + " is not Some with value" + q(expected),
             value)
    }
  }
  def some[T](t: =>T) = beSome(t)
  def beSome[T] = new SomeMatcher[T]
  def some[T] = beSome
  def beNone = new Matcher[Option[Any]] {
    def apply[S <: Option[Any]](value: Expectable[S]) = {
      result(value.value == None,
             value.description + " is None",
             value.description + " is not None",
             value)
    }
  }
  def none = beNone
  def beAsNoneAs[T](other: =>Option[T]) = new Matcher[Option[T]] {
    def apply[S <: Option[T]](a: Expectable[S]) = {
      val b = other
      result(a.value == None && b == None || a.value != None && b != None, 
             a.description + " is None as well",
             if (a.value == None) b + " is not None" else a.description + " is not None",
             a)
    }
  }
  def asNoneAs[T](other: =>Option[T]) = beAsNoneAs(other)
}
private[specs2]
trait OptionBeHaveMatchers { outer: OptionBaseMatchers =>
  implicit def toOptionResultMatcher[T](result: MatchResult[Option[T]]) = new OptionResultMatcher(result)
  class OptionResultMatcher[T](result: MatchResult[Option[T]]) {
    def some = result(outer.beSome)
    def some(t: =>T) = result(outer.beSome(t))
    def none = result(outer.beNone)
    def asNoneAs(other: =>Option[T]) = result(beAsNoneAs(other))
  }
}
private[specs2]
class SomeMatcher[T] extends Matcher[Option[T]] {
  def apply[S <: Option[T]](value: Expectable[S]) = {
    result(value.value.map(t => true).getOrElse(false),
           value.description + " is Some[T]",
           value.description + " is not Some[T]",
           value)
  }
  def which(f: T => Boolean) = this ^^ { (t: Option[T]) => t filter f }
}
