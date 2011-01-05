package org.specs2
package matcher

import scalaz.Scalaz
import scalaz.Scalaz._
import execute._
import Expectable._
import text.Quote._
import reflect.ClassName._
import MatchResult._
import time.Duration

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
  def apply[S <: T](t: Expectable[S]): MatchResult[S]
  
  /**
   * This convenience method can be used to evaluate a boolean condition and return the
   * appropriate MatchResult, depending on the boolean value
   * @return a MatchResult
   */
  protected def result[S <: T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: Expectable[S]): MatchResult[S] = {
	  Matcher.result(test, okMessage, koMessage, value) 
  }
  /**
   * This convenience method can be used to evaluate a boolean condition and return the
   * appropriate MatchResult, depending on the boolean value
   * @return a MatchResult
   */
  protected def result[S <: T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: Expectable[S], expected: String, actual: String): MatchResult[S] = {
	  Matcher.result(test, okMessage, koMessage, value, expected, actual)
  }
  /**
   * This convenience method can be used to evaluate a boolean condition and return the
   * appropriate MatchResult, depending on the boolean value
   * @return a MatchResult
   */
  protected def result[S <: T](other: MatchResult[_], value: Expectable[S]): MatchResult[S] = {
    other match {
      case MatchSuccess(ok, ko, _) => Matcher.result(true, ok, ko, value)
      case MatchFailure(ok, ko, _, NoDetails()) => Matcher.result(false, ok, ko, value)
      case MatchFailure(ok, ko, _, FailureDetails(expected, actual)) => Matcher.result(false, ok, ko, value, expected, actual)
      case _  => Matcher.result(other.isSuccess, other.message, other.message, value)
    }
  }

  protected def result[S <: T](other: MatchResultMessage, value: Expectable[S]): MatchResult[S] = {
    val (okMessage, koMessage) = other match {
      case SuccessMessage(ok, ko) => (ok, ko)
      case FailureMessage(ok, ko) => (ok, ko)
      case NeutralMessage(message)  => (message, message)
      case EmptyMessage() => ("", "") 
    }
    Matcher.result(other.isSuccess, okMessage, koMessage, value) 
  }
 
  /** 
   * Adapts a matcher to another.
   * ex: `be_==("message") ^^ (_.getMessage)` can be applied to an exception
   */
  def ^^[S](f: S => T) = new Matcher[S] {
    override val nameOption = outer.nameOption
    def apply[U <: S](a: Expectable[U]) = {
      val result = outer.apply(a.map(f))
      result.map((t: T) => a.value)
    }
  }

  /**
   * negate a Matcher
   * @see MatchResult.not
   */
  def not = new Matcher[T] {
    override val nameOption = outer.nameOption.map("not "+_)
    def apply[U <: T](a: Expectable[U]) = outer(a).not
  }
  /**
   * the logical and between 2 matchers
   * @see MatchResult.and
   */
  def and[S <: T](m: =>Matcher[S]): Matcher[S] = new Matcher[S] {
    override val nameOption = outer.nameOption.map(_+" and "+m.name)
    def apply[U <: S](a: Expectable[U]) = outer(a).and(m(a))
  }
  /**
   * the logical or between 2 matchers
   * @see MatchResult.or
   */
  def or[S <: T](m: =>Matcher[S]) = new Matcher[S] {
    override val nameOption = outer.nameOption.map(_+" or "+m.name)
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
  def orSkip(m: String): Matcher[T] = new Matcher[T] {
    override val nameOption = outer.nameOption
    def apply[U <: T](a: Expectable[U]) = {
      outer(a) match {
    	  case MatchFailure(_, ko, _, d) => MatchSkip(m prefix(": ", ko), a)
    	  case other => other
      }
    }
  }
  /**
   *  The <code>lazily</code> operator returns a matcher which will match a function returning the expected value
   */   
  def lazily = new Matcher[() => T]() {
    override val nameOption = outer.nameOption
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
   *  @return a matcher that needs to eventually match, after a given number of retries 
   *  and a sleep time
   */
  def eventually(retries: Int, sleep: Duration): Matcher[T] = EventuallyMatchers.eventually(retries, sleep)(this)

  /**
   * @return the name of the matcher based on the class name of its first parent which is not an anonymous class
   */
  def name = nameOption.getOrElse(humanName(getClass))

  /**
   * this can be provided with the withName method
   */
  val nameOption: Option[String] = None

  /**
   * give a name to the matcher when it's built from another one
   */
  def withName(name: String) = new Matcher[T] {
    override val nameOption = Some(name)
    def apply[U <: T](a: Expectable[U]) = outer(a)
  }
}

/**
 *  Inherit this trait to provide a Matcher where both the actual and the expected values can be adapted with a function.
 */
trait AdaptableMatcher[T] extends Matcher[T] { outer =>
  /** 
   * @return a matcher changing its expected value, possibly adding more information to
   *         the ok and ko messages
   */
  def adapt(f: T => T, ok: String => String = identity, ko: String => String = identity): AdaptableMatcher[T]
  /** 
   * Adapts a matcher with both the expected and actual values
   * ex: `be_==("message") ^^^ (_.trim)` will do the comparison on both trimmed 
   * strings  
   */
  def ^^^(f: T => T, ok: String => String = identity, ko: String => String = identity): AdaptableMatcher[T] =
    new AdaptableMatcher[T] {
      override val nameOption = outer.nameOption

      def adapt(g: T => T, okFunction: String => String, koFunction: String => String): AdaptableMatcher[T] =
        outer.adapt(g compose f, okFunction compose ok, koFunction compose ko)
        
      def apply[U <: T](a: Expectable[U]) = {
        val result = outer.adapt(f, ok, ko).apply(a.map(f))
        result.map((t: T) => a.value)
      }
    }
  def ^^(f: T => T) = new AdaptableMatcher[T] {
    override val nameOption = outer.nameOption

    def adapt(f2: T => T, ok: String => String = identity, ko: String => String = identity) =
      outer.adapt(f2, ok, ko)
      
    def apply[U <: T](a: Expectable[U]) = {
      val result = outer.apply(a.map(f))
      result.map((t: T) => a.value)
    }
  }
}

object Matcher {
  /**
   * Utility method for creating a MatchResult[T]
   */
  def result[T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: Expectable[T]): MatchResult[T] = {
	  if (test) new MatchSuccess(okMessage, koMessage, value) 
	  else new MatchFailure(okMessage, koMessage, value)
  }
  /**
   * Utility method for creating a MatchResult[T], with the actual and expected strings to enable better failure
   * messages
   */
  def result[T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: Expectable[T], expected: String, actual: String): MatchResult[T] = {
	  if (test) new MatchSuccess(okMessage, koMessage, value)
	  else new MatchFailure(okMessage, koMessage, value, FailureDetails(expected, actual))
  }
}
