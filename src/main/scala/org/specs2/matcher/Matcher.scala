package org.specs2
package matcher

import scalaz.Scalaz
import scalaz.Scalaz._
import execute._
import Expectable._
import MatchResult._
/**
 * The `Matcher` trait is the base trait for any Matcher.
 * 
 * This trait can be extended to provide an appropriate <code>apply</code> method that 
 * will check an expectable value `a: Expectable[T]`.
 *
 * The result of a match is a MatchResult object (@see MatchResult).
 * 
 * Matchers can be composed
 * 
 * Implementation notes:
 *   * the parameter to the apply method must be a by-name parameter. 
 *     This allows some values to be evaluated only when necessary. 
 *     
 *   * However in the implementation of the apply function, it must be taken care of not 
 *     evaluating the parameter twice. Assigning it to a val is the solution to this issue.
 */
trait Matcher[-T] { outer =>

  /** 
   * apply this matcher to an Expectable
   * @return a MatchResult describing the outcome of the match
   */
  def apply[S <: T](t: =>Expectable[S]): MatchResult[S]
  
  /**
   * This convenience method can be used to evaluate a boolean condition and return the 
   * appropriate MatchResult, depending on the boolean value
   * @return a MatchResult
   */
  protected def result[S <: T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: Expectable[S]): MatchResult[S] = {
	  Matcher.result(test, okMessage, koMessage, value) 
  }
 
  /** 
   * Adapts a matcher to another.
   * ex: `be_==("message") ^^ (_.getMessage)` can be applied to an exception  
   */
  def ^^[S](f: S => T) = new Matcher[S] {
    def apply[U <: S](a: =>Expectable[U]) = {
      val result = outer.apply(a.map(f))
      result.map((t: T) => a.value)
    }
  }

  /**
   * negate a Matcher
   * @see MatchResult.not
   */
  def not = new Matcher[T] {
    def apply[U <: T](a: =>Expectable[U]) = outer(a).not
  }
  /**
   * the logical or between 2 matchers
   * @see MatchResult.or
   */
  def or[S <: T](m: =>Matcher[S]) = new Matcher[S] {
    def apply[U <: S](a: =>Expectable[U]) = {
      val value = a
      outer(value).or(m(value))
    }
  }
  /**
   * @return a Skip MatchResult if this matcher fails
   */
  def orSkip: Matcher[T] = new Matcher[T] {
    def apply[U <: T](a: =>Expectable[U]) = {
      val value = a
      outer(value) match {
    	  case MatchFailure(_, ko, _) => MatchSkip(ko, value)
    	  case other => other
      }
    }
  }
}

object Matcher {
  def result[T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: =>Expectable[T]): MatchResult[T] = {
	  if (test) new MatchSuccess(okMessage, koMessage, value) 
	  else new MatchFailure(okMessage, koMessage, value)
  }
}
