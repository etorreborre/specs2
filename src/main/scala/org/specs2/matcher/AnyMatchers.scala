package org.specs2
package matcher

import reflect.Classes._
import text.Quote._
import execute._

/**
 * This trait provides matchers which are applicable to any type of value
 */
trait AnyMatchers extends AnyBaseMatchers with AnyBeHaveMatchers
object AnyMatchers extends AnyMatchers

private[specs2]
trait AnyBaseMatchers {

  /** matches if a == true */
  def beTrue = new BeTrueMatcher
  /** matches if a == false */
  def beFalse = beTrue.not

  /** matches if a eq b */
  def be[T <: AnyRef](t: =>T) = new Matcher[T] {
    def apply[S <: T](v: =>Expectable[S]) = {
      val (a, b) = (v, t)
      result(a.value eq b, a.description + " is " + q(b), a.description + " is not " + q(b), a) 
    }
  }

  /** matches if a == b */
  def be_==[T](t: =>T) = beEqualTo(t)
  /** matches if a == b */
  def beEqualTo[T](t: =>T) = new BeEqualTo(t)
  /** matches if a == b */
  def equalTo[T](t: =>T) = beEqualTo(t)

  /** negate a matcher */
  def not[T](m: Matcher[T]) = m.not
  
  /** matches if a.isEmpty */
  def empty[T <: Any { def isEmpty: Boolean }] = beEmpty[T]
  /** matches if a.isEmpty */
  def beEmpty[T <% Any { def isEmpty: Boolean }] = new Matcher[T] {
    def apply[S <: T](v: =>Expectable[S]) = {
      val iterable = v
      result(iterable.value.isEmpty, 
             iterable.description + " is empty", 
             iterable.description + " is not empty", iterable)
    }
  }
  def beLike[T](pattern: PartialFunction[T, MatchResult[_]]) = new Matcher[T] {
    def apply[S <: T](v: =>Expectable[S]) = {
      val a = v
      
      val r = if (a.value != null && pattern.isDefinedAt(a.value)) pattern.apply(a.value) else MatchFailure("", "", a)
      result(r.isSuccess,
             a.description + " matches the given pattern " + r.message,
             a.description + " doesn't match the expected pattern "  + r.message,
             a)
    }
  }
  def like[T](pattern: =>PartialFunction[T, MatchResult[_]]) = beLike(pattern)
  /** @alias for beLike */
  def beLikeA[T](pattern: =>PartialFunction[T, MatchResult[_]]) = beLike(pattern)
  def likeA[T](pattern: =>PartialFunction[T, MatchResult[_]]) = beLike(pattern)
}
/**
 * Matcher for a boolean value
 */
class BeTrueMatcher extends Matcher[Boolean] {
  def apply[S <: Boolean](v: =>Expectable[S]) = {
    result(v.value, v.description + " is true", v.description + " is false", v) 
  }
}
/**
 * Equality Matcher
 */
class BeEqualTo[T](t: =>T) extends AdaptableMatcher[T] { outer =>
  import AnyMatchers._
  protected val ok: String => String = identity
  protected val ko: String => String = identity
  
  def adapt(f: T => T, okFunction: String => String, koFunction: String => String) = {
    val newMatcher = new BeEqualTo(f(t)) {
      override protected val ok: String => String = okFunction compose outer.ok
      override protected val ko: String => String = koFunction compose outer.ko
    } 
    newMatcher.^^((t: T) => f(t))
  }
  
  def apply[S <: T](v: =>Expectable[S]) = {
    val (a, b) = (t, v)
    val (db, qa) = (b.description, q(a)) match {
      case (x, y) if (a != b && q(a) == q(b)) => {
	      val aClass = getClassName(x)
	      val bClass = getClassName(y)
	      if (aClass != bClass)
          (y + ": " + bClass, x + ": " + aClass)
        else
          (y, x + ". Values have the same string representation but possibly different types like List[Int] and List[String]")
	    }
      case other @ _ => other 
	  }
    result(a == b.value, ok(db + " is equal to " + qa), ko(db + " is not equal to " + qa), b)
  }
}

/**
 * This trait allows to write expressions like
 * 
 *  `1 must be equalTo(1)`
 */
trait AnyBeHaveMatchers { outer: AnyMatchers =>
  implicit def anyBeHaveMatcher[T](s: MatchResult[T]) = new AnyBeHaveMatchers(s)
  class AnyBeHaveMatchers[T](s: MatchResult[T]) {
    def equalTo(t: T) = s.apply(outer.be_==(t))
  }
  
  implicit def anyWithEmpty[T <% Any { def isEmpty: Boolean }](s: MatchResult[T]) = 
    new AnyWithEmptyMatchers(s)
  class AnyWithEmptyMatchers[T <% Any { def isEmpty: Boolean }](s: MatchResult[T]) {
    def empty = s.apply(outer.beEmpty[T])
    def beEmpty = s.apply(outer.beEmpty[T])
  }
  implicit def toBeLikeResultMatcher[T](result: MatchResult[T]) = new BeLikeResultMatcher(result)
  class BeLikeResultMatcher[T](result: MatchResult[T]) {
    def like(pattern: =>PartialFunction[T, MatchResult[_]]) = result(outer.beLike(pattern))
    def likeA(pattern: =>PartialFunction[T, MatchResult[_]]) = result(outer.beLike(pattern))
  }
}