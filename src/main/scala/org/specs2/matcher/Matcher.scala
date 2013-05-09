package org.specs2
package matcher

import scalaz.Scalaz._
import control.Exceptions._
import execute._
import Expectable._
import text.Quote._
import text.Plural._
import reflect.ClassName._
import MatchResultMessages._
import time.Duration
import MatchResultLogicalCombinators._
import scala.collection.GenTraversableOnce

/**
 * The `Matcher` trait is the base trait for any Matcher.
 * 
 * This trait can be extended to provide an appropriate `apply` method that
 * will check an expectable value `a: Expectable[T]`.
 *
 * The result of a match is a MatchResult object (@see MatchResult).
 * 
 * Matchers can be composed.
 * 
 * Implementation notes:
 *   - the parameter to the apply method must be a by-name parameter.
 *     This allows some values to be evaluated only when necessary.
 *     
 *   - However in the implementation of the apply function, it must be taken care of not
 *     evaluating the parameter twice. Assigning it to a val is the solution to this issue.
 */
trait Matcher[-T] { outer =>

  /** 
   * apply this matcher to an Expectable
   * @return a MatchResult describing the outcome of the match
   */
  def apply[S <: T](t: Expectable[S]): MatchResult[S]
  
  /**
   * This convenience method can be used to evaluate a boolean condition and return an appropriate MatchResult
   * @return a MatchResult with an okMessage, a koMessage and the expectable value
   */
  protected def result[S <: T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: Expectable[S]): MatchResult[S] = {
	  Matcher.result(test, okMessage, koMessage, value) 
  }
  /**
   * This convenience method uses a triplet instead of separated arguments
   * @return a MatchResult with an okMessage, a koMessage and the expectable value
   */
  protected def result[S <: T](triplet: =>(Boolean, String, String), value: Expectable[S]): MatchResult[S] = {
    Matcher.result(triplet._1, triplet._2, triplet._3, value)
  }
  /**
   * This convenience method can be used to evaluate a boolean condition and return an appropriate MatchResult
   * @return a MatchResult with an okMessage, a koMessage, the expectable value and the expected/actual values as string
   *         to display a failure comparison if necessary
   */
  protected def result[S <: T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: Expectable[S], expected: String, actual: String): MatchResult[S] = {
	  Matcher.result(test, okMessage, koMessage, value, expected, actual)
  }
  /**
   * @return a MatchResult copied on another one, but with a different expectable
   */
  protected def result[S <: T](other: MatchResult[_], value: Expectable[S]): MatchResult[S] = {
    other match {
      case MatchSuccess(ok, ko, _)                                   => Matcher.result(true,  ok(), ko(), value)
      case MatchFailure(ok, ko, _, NoDetails())                      => Matcher.result(false, ok(), ko(), value)
      case MatchFailure(ok, ko, _, FailureDetails(expected, actual)) => Matcher.result(false, ok(), ko(), value, expected, actual)
      case _                                                         => Matcher.result(other.isSuccess, other.message, other.message, value)
    }
  }
  /**
   * @return a MatchResult using the messages embedded in a MatchResultMessage (i.e. an accumulation of messages from other matches)
   */
  protected def result[S <: T](other: MatchResultMessage, value: Expectable[S]): MatchResult[S] = {
    lazy val messages = other match {
      case SuccessMessage(ok, ko)  => (ok(), ko())
      case FailureMessage(ok, ko)  => (ok(), ko())
      case NeutralMessage(message) => (message, message)
      case EmptySuccessMessage()   => ("", "")
    }
    Matcher.result(other.isSuccess, messages._1, messages._2, value)
  }
 
  /**
   * Adapt a matcher to another.
   * ex: `be_==("message") ^^ (_.getMessage)` can be applied to an exception
   */
  def ^^[S](f: S => T) = new Matcher[S] {
    def apply[U <: S](a: Expectable[U]) = {
      val result = outer.apply(a.map(f))
      result.map((t: T) => a.value)
    }
  }
  /**
   * Adapt a matcher to another.
   * ex: `be_==("message") ^^ (_.getMessage aka "trimmed")` can be applied to an exception
   *
   * The dummy value is used to help to disambiguate with the overloaded ^^ function
   */
  def ^^[S](f: S => Expectable[T], dummy: Int = 0) = new Matcher[S] {
    def apply[U <: S](a: Expectable[U]) = {
      val result = outer.apply(a.flatMap(f))
      result.map((t: T) => a.value)
    }
  }
  /**
   * negate a Matcher
   * @see MatchResult.not
   */
  def not = new Matcher[T] {
    def apply[U <: T](a: Expectable[U]) = outer(a).not
  }
  /**
   * the logical and between 2 matchers
   * @see MatchResult.and
   */
  def and[S <: T](m: =>Matcher[S]): Matcher[S] = new Matcher[S] {
    def apply[U <: S](a: Expectable[U]) = outer(a).and(m(a))
  }
  /**
   * the logical or between 2 matchers
   * @see MatchResult.or
   */
  def or[S <: T](m: =>Matcher[S]) = new Matcher[S] {
    def apply[U <: S](a: Expectable[U]) = outer(a).or(m(a))
  }
  /**
   * @return a Skip MatchResult if this matcher fails
   */
  def orSkip: Matcher[T] = orSkip("")
  /**
   * @return a Skip MatchResult if this matcher fails, prefixing the failure message with a skip message.
   * If the skip message is empty, only the failure message is printed
   */
  def orSkip(m: String): Matcher[T] = orSkip((ko: String) => m prefix(": ", ko))

  /**
   * @return a Skip MatchResult if this matcher fails, modifying the failure message with a skip message.
   */
  def orSkip(message: String => String): Matcher[T] = new Matcher[T] {
    def apply[U <: T](a: Expectable[U]) = {
      tryOr(outer(a)) { (e: Exception) => MatchSkip(message(e.getMessage), a) } match {
        case MatchFailure(_,ko,_,_)    => MatchSkip(message(ko()), a)
        case other                     => other
      }
    }
  }
  /**
   * @return a Pending MatchResult if this matcher fails
   */
  def orPending: Matcher[T] = orPending("")
  /**
   * @return a Pending MatchResult if this matcher fails, prefixing the failure message with a pending message.
   * If the pending message is empty, only the failure message is printed
   */
  def orPending(m: String): Matcher[T] = orPending((ko: String) => m prefix(": ", ko))

  /**
   * @return a Pending MatchResult if this matcher fails, modifying the failure message with a pending message.
   */
  def orPending(message: String => String): Matcher[T] = new Matcher[T] {
    def apply[U <: T](a: Expectable[U]) = {
      tryOr(outer(a)) { (e: Exception) => MatchPending(message(e.getMessage), a) } match {
        case MatchFailure(_,ko,_,_)    => MatchPending(message(ko()), a)
        case other                     => other
      }
    }
  }

  /** only apply this matcher if the condition is true */
  def when(b: Boolean, m: String= ""): Matcher[T] = new Matcher[T] {
    def apply[U <: T](a: Expectable[U]) = if (b) outer(a) else MatchSuccess(m, "ko", a)
  }
  /** only apply this matcher if the condition is false */
  def unless(b: Boolean, m: String= ""): Matcher[T] = when(!b, m)
  /** when the condition is true the matcher is applied, when it's false, the matcher must fail */
  def iff(b: Boolean): Matcher[T] = new Matcher[T] {
    def apply[U <: T](a: Expectable[U]) = if (b) outer(a) else outer(a).not
  }
  /**
   *  The `lazily` operator returns a Matcher which will match a function returning the expected value
   */   
  def lazily = new Matcher[() => T]() {
    def apply[S <: () => T](function: Expectable[S]) = {
      val r = outer(Expectable(function.value()))
      result(r, function)
    } 
  }
  /** 
   * @return a matcher that needs to eventually match, after 40 retries and a sleep time 
   * of 100 milliseconds
   */
  def eventually: Matcher[T] = EventuallyMatchers.eventually(this)
  /**
   * @return a matcher that needs to eventually match, after a given number of retries
   * and a sleep time
   */
  def eventually(retries: Int, sleep: Duration): Matcher[T] = EventuallyMatchers.eventually(retries, sleep)(this)

  /**
   * @return a Matcher matching all the elements of a sequence against the current matcher, stopping after the first
   * failure
   */
  def forall = new Matcher[Traversable[T]] {
    def apply[S <: Traversable[T]](seq: Expectable[S]) =
      MatchersImplicits.verifyFunction((t: T) => outer.apply(Expectable(t))).forall(seq.value)
  }
  /**
   * @return a Matcher matching all the elements of a sequence against the current matcher, cumulating all failures
   */
  def foreach = new Matcher[Traversable[T]] {
    def apply[S <: Traversable[T]](seq: Expectable[S]) =
      MatchersImplicits.verifyFunction((t: T) => outer.apply(Expectable(t))).foreach(seq.value)
  }

  /**
   * @return a Matcher matching at least one element of a sequence against the current matcher
   */
  def atLeastOnce = new Matcher[GenTraversableOnce[T]] {
    def apply[S <: GenTraversableOnce[T]](seq: Expectable[S]) = {
      val r = MatchersImplicits.verifyFunction((t: T) => outer.apply(Expectable(t))).atLeastOnce(seq.value)
      result(r, seq)
    }
  }

  /**
   * @return a Matcher with no messages
   */
  def mute = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]) = outer.apply(s).mute
  }

  /**
   * @return update the failure message of a matcher
   */
  def updateMessage(f: String => String) = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]) = outer.apply(s).updateMessage(f)
  }
  /**
   * @return set a new failure message of a matcher
   */
  def setMessage(message: String) = updateMessage((s: String) => message)

  /**
   * @return a test function corresponding to this matcher
   */
  def test = (t: T) => apply(Expectable(t)).isSuccess
}

object Matcher {
  /**
   *  Utility method for creating a MatchResult[T]
   */
  def result[T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: Expectable[T]): MatchResult[T] = {
	  if (test) MatchSuccess(okMessage, koMessage, value)
	  else      MatchFailure(okMessage, koMessage, value)
  }
  /**
   * Utility method for creating a MatchResult[T], with the actual and expected strings to enable better failure
   * messages
   */
  def result[T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: Expectable[T], expected: String, actual: String): MatchResult[T] = {
	  if (test) MatchSuccess(okMessage, koMessage, value)
	  else      MatchFailure.create(okMessage, koMessage, value, FailureDetails(expected, actual))
  }
}
