package org.specs2
package matcher

import org.specs2.control.Exceptions._
import org.specs2.execute._, Result._
import execute.ResultImplicits._
import execute.ResultLogicalCombinators._
import org.specs2.text.NotNullStrings._
import org.specs2.text.Quote._
import org.specs2.text.Sentences._

import scala.concurrent.duration._
import org.specs2.fp.syntax._
import annotation._

/**
 * The `Matcher` trait is the base trait for any Matcher.
 *
 * This trait can be extended to provide an appropriate `apply` method that
 * will check an expectable value `a: Expectable[T]`.
 *
 * The result of a match is a Result
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
trait Matcher[-T]:
  outer =>

  /**
   * apply this matcher to an Expectable
   * @return a Result describing the outcome of the match
   */
  def apply[S <: T](t: Expectable[S]): Result

  /**
   * Adapt a matcher to another.
   * ex: `be_==("message") ^^ (_.getMessage)` can be applied to an exception
   */
  def ^^[S](f: S => T): Matcher[S] =
    new Matcher[S]:
      def apply[U <: S](a: Expectable[U]): Result =
        outer(a.map(f))

  /**
   * Adapt a matcher to another.
   * ex: `be_==("message") ^^ (_.getMessage aka "trimmed")` can be applied to an exception
   *
   * The dummy value is used to help to disambiguate with the overloaded ^^ function
   */
  def ^^[S](f: S => Expectable[T], dummy: Int = 0): Matcher[S] =
    new Matcher[S]:
      def apply[U <: S](a: Expectable[U]): Result =
        f(a.value).applyMatcher(outer)

  /**
   * negate a Matcher
   * @see Result.not
   */
  def not: Matcher[T] =
    new Matcher[T]:
      def apply[U <: T](a: Expectable[U]): Result =
        val result =
          try outer(a)
          catch
            case FailureException(f: Failure) => f
        a.checker.check(result.not)

  /**
   * the logical and between 2 matchers
   * @see Result.and
   */
  def and[S <: T](m: =>Matcher[S]): Matcher[S] =
    new Matcher[S]:
      def apply[U <: S](a: Expectable[U]): Result =
        outer(a).and(m(a))

  /**
   * the logical or between 2 matchers
   * @see Result.or
   */
  def or[S <: T](m: =>Matcher[S]): Matcher[S] =
    new Matcher[S]:
      def apply[U <: S](a: Expectable[U]): Result =
        outer(a).or(m(a))

  /**
   * @return a Skipped result if this matcher fails
   */
  def orSkip: Matcher[T] =
    orSkip("")

  /**
   * @return a Skipped Result if this matcher fails, prefixing the failure message with a skip message.
   * If the skip message is empty, only the failure message is printed
   */
  def orSkip(m: String): Matcher[T] =
    orSkip((ko: String) => m prefix(": ", ko))

  /**
   * @return a Skipped Result if this matcher fails, modifying the failure message with a skip message.
   */
  def orSkip(message: String => String): Matcher[T] =
    new Matcher[T]:
      def apply[U <: T](a: Expectable[U]) =
        tryOr(outer(a)) { t => Skipped(message(t.getMessage.notNull)) } match
            case f: Failure =>
              Skipped(message(f.message))
            case other =>
              other
  /**
   * throw a FailureException if this matcher fails
   */
  def orThrow: Matcher[T] =
    orThrow("")

  /**
   * throw a FailureException if this matcher fails prefixing the failure message with a message.
   * If the message is empty, only the failure message is printed
   */
  def orThrow(m: String): Matcher[T] =
    orThrow((ko: String) => m prefix(": ", ko))

  /**
   * throw a FailureException if this matcher fails, modifying the failure message with a message.
   */
  def orThrow(message: String => String): Matcher[T] =
    new Matcher[T]:
      def apply[U <: T](a: Expectable[U]): Result =
        tryOr(outer(a)) { t => throw new FailureException(Failure(message(t.getMessage.notNull))) } match
          case f: Failure =>
            throw new FailureException(f)
          case other =>
            other

  /**
   * @return a Pending Result if this matcher fails
   */
  def orPending: Matcher[T] =
    orPending("")

  /**
   * @return a Pending Result if this matcher fails, prefixing the failure message with a pending message.
   * If the pending message is empty, only the failure message is printed
   */
  def orPending(m: String): Matcher[T] =
    orPending((ko: String) => m prefix(": ", ko))

  /**
   * @return a Pending Result if this matcher fails, modifying the failure message with a pending message.
   */
  def orPending(message: String => String): Matcher[T] =
    new Matcher[T] {
      def apply[U <: T](a: Expectable[U]) =
        tryOr(outer(a)) { t => Pending(message(t.getMessage.notNull)) } match
          case f: Failure =>
            Pending(message(f.message))
          case other =>
            other
    }

  /** only apply this matcher if the condition is true */
  def when(b: Boolean, m: String = ""): Matcher[T] =
    new Matcher[T]:
      def apply[U <: T](a: Expectable[U]): Result =
        if b then outer(a) else Success(m)


  /** only apply this matcher if the condition is false */
  def unless(b: Boolean, m: String= ""): Matcher[T] =
    when(!b, m)

  /** when the condition is true the matcher is applied, when it's false, the matcher must fail */
  def iff(b: Boolean): Matcher[T] =
    new Matcher[T]:
      def apply[U <: T](a: Expectable[U]) =
        if b then outer(a) else outer(a).not

  /**
   *  The `lazily` operator returns a Matcher which will match a function returning the expected value
   */
  def lazily: Matcher[() => T] =
    new Matcher[() => T]:
      self =>
      def apply[S <: () => T](function: Expectable[S]) =
        outer(createExpectable(function.value()))

  /**
   * @return a matcher that needs to eventually match, after 40 retries and a sleep time
   * of 100 milliseconds
   */
  def eventually: Matcher[T] =
    EventuallyMatchers.eventually(this)

  /**
   * @return a matcher that needs to eventually match, after a given number of retries
   * and a sleep time
   */
  def eventually(retries: Int, sleep: Duration): Matcher[T] =
    EventuallyMatchers.eventually(this, retries, sleep)

  /**
   * @param sleep the function applied on the retry number (first is 1)
   * @return a matcher that needs to eventually match, after a given number of retries
   * and a sleep time
   *
   * {{{
   * (aResult === expected).eventually(retries = 2, _ * 100.milliseconds)
   * }}}
   */
  def eventually(retries: Int, sleep: Int => Duration): Matcher[T] =
    EventuallyMatchers.eventually(this, retries, sleep)

  /**
   * @return a Matcher with no messages
   */
  def mute = setMessage("")

  /**
   * @return update the failure message of a matcher
   */
  def updateMessage(f: String => String): Matcher[T] =
    new Matcher[T] {
      def apply[S <: T](s: Expectable[S]) =
        try outer.apply(s).updateMessage(f)
        catch { case FailureException(Failure(m, e, st, d)) =>
          throw FailureException(Failure(f(m), e, st, d)) }
    }

  /**
   * @return set a new failure message of a matcher
   */
  def setMessage(message: String): Matcher[T] =
    updateMessage(_ => message)

  /**
   * @return a test function corresponding to this matcher
   */
  def test: T => Boolean =
    (t: T) => apply(Expectations.createExpectable(t)).isSuccess

end Matcher

object Matcher extends MatcherCreation:

  @targetName("fromCondition")
  def apply[T](f: (T => Boolean, String)): Matcher[T] = f
  @targetName("fromConditionActualExpected")
  def apply[T](f: (T => Boolean, String, String, String)): Matcher[T] = f
  @targetName("fromFunctions")
  def apply[T](f: (T => Boolean, T => String)): Matcher[T] = f
  @targetName("fromFunctionsActualExpected")
  def apply[T](f: (T => Boolean, T => String, T => String, T => String)): Matcher[T] = f
  @targetName("fromFunction")
  def apply[T](f: T => (Boolean, String)): Matcher[T] = f
  @targetName("fromFunctionActualExpected")
  def apply[T](f: T => (Boolean, String, String, String)): Matcher[T] = f
  def apply[T, R : AsResult](f: T => R): Matcher[T] = f
  def apply[T, R : AsResult](f: T => Matcher[R]): (=>T) => Matcher[R] = f

trait MatcherCreation:

  /**
   * This method transforms a function to a Matcher
   */
  given [T] as Conversion[(T => Boolean, String), Matcher[T]]:
    def apply(f: (T => Boolean, String)): Matcher[T] =
      (f._1, (t:T) => q(t)+" "+f._2)

  /**
   * This method transforms a function to a Matcher
   */
  given [T] as Conversion[(T => Boolean, String, String, String), Matcher[T]]:
    def apply(f: (T => Boolean, String, String, String)): Matcher[T] =
      (f._1, (t:T) => q(t)+" "+f._2, (_:T) => f._3, (_:T) => f._4)

  /**
   * This method transforms a function to a Matcher
   */
  given [T] as Conversion[(T => Boolean, T => String), Matcher[T]]:
    def apply(f: (T => Boolean, T => String)): Matcher[T] =
      new Matcher[T]:
        def apply[S <: T](s: Expectable[S]) =
          val (condition, message) = (f._1(s.value), f._2(s.value))
          Result.result(condition, message)

  /**
   * This method transforms a function, with function descriptors to a Matcher
   */
  given [T] as Conversion[(T => Boolean, T => String, T => String, T => String), Matcher[T]]:
   def apply(f: (T => Boolean, T => String, T => String, T => String)): Matcher[T] =
     new Matcher[T]:
       def apply[S <: T](s: Expectable[S]) =
         val (condition, message, actual, expected) = (f._1(s.value), f._2(s.value), f._3(s.value), f._4(s.value))
         Result.result(condition, message, actual, expected)

  /**
   * This method transforms a function returning a pair (condition, message) to a Matcher
   */
  given pairFunctionToMatcher[T] as Conversion[T => (Boolean, String), Matcher[T]]:
   def apply(f: T => (Boolean, String)): Matcher[T] =
     new Matcher[T]:
       def apply[S <: T](s: Expectable[S]) =
          val (condition, message) = f(s.value)
          Result.result(condition, message)

  /**
   * This method transforms a function returning a triplet (condition, message, actual, expected) to a Matcher
   */
  given [T] as Conversion[T => (Boolean, String, String, String), Matcher[T]]:
    def apply(f: T => (Boolean, String, String, String)): Matcher[T] =
      new Matcher[T]:
        def apply[S <: T](s: Expectable[S]) =
          val (condition, message, actual, expected) = f(s.value)
          Result.result(condition, message, actual, expected)

  /**
   * This method transforms a function returning a Result to a Matcher
   */
  given resultFunctionToMatcher[T, R : AsResult] as Conversion[T => R, Matcher[T]]:
    def apply(f: T => R): Matcher[T] =
      new Matcher[T]:
        def apply[S <: T](s: Expectable[S]) =
          ResultExecution.execute(AsResult(f(s.value)))

  /** this allows a function returning a matcher to be used where the same function with a byname parameter is expected */
  given matcherFunctionToMatcher[T, R] as Conversion[T => Matcher[R], (=>T) => Matcher[R]]:
    def apply(f: T => Matcher[R]): (=>T) => Matcher[R] =
      def f1(t: =>T) = f(t)
      f1


object MatcherCreation extends MatcherCreation
