package org.specs2
package matcher

import execute._
import text.Quote._
import scala.collection.Traversable
import ResultLogicalCombinators.{given, _}
import text.Sentences._
import control._
import ValueChecks.{given}

/**
* This trait provides implicit definitions from MatchResults and Booleans to Results.
*
* It also allows to:
*
* - create matchers from functions
* - create matchers for seqs and sets from single matchers
*/
trait MatchersImplicits extends
       ResultImplicits
  with MatchResultImplicits
  with MatchersCreation
  with SequenceMatchersCreation

object MatchersImplicits extends MatchersImplicits

/**
 * Implicit conversions for MatchResults
 */
trait MatchResultImplicits extends MatchResultCombinators:
  /**
   * implicit definition to transform a Seq of MatchResults to a Result
   */
  given [T] as Conversion[Seq[MatchResult[T]], Result]:
    def apply(r: Seq[MatchResult[T]]): Result =
      r.foldLeft(StandardResults.success: Result)(_ and _.toResult)

  /**
   * implicit definition to transform any MatchResult to a Result
   */
  given [T] as Conversion[MatchResult[T], Result]:
    def apply(r: MatchResult[T]): Result =
      ResultExecution.execute(r.toResult)

  /**
   * implicit definition to accept any MatchResult as a Boolean value.
   * It is true if the MatchResult is not an Error or a Failure
   */
  implicit def matchResultToBoolean(res: =>MatchResult[_]): Boolean =
    val r = res
    r.isSuccess || r.toResult.isSkipped || r.toResult.isPending


object MatchResultImplicits extends MatchResultImplicits

trait ResultImplicits extends ExpectationsCreation:

  /**
   * Extend collections to check all their elements
   */
  extension [T, R : AsResult](values: Traversable[T]):

    def forall(f: T => R): MatchResult[Traversable[T]] =
      createExpectable(values).applyMatcher(ContainWithResult(f).forall)

    def atLeastOnce(f: T => R): MatchResult[Traversable[T]] =
      createExpectable(values).applyMatcher(ContainWithResult(f).atLeastOnce)

    def atMostOnce(f: T => R): MatchResult[Traversable[T]] =
      createExpectable(values).applyMatcher(ContainWithResult(f).atMostOnce)

    def atLeast(n: Times)(f: T => R): MatchResult[Traversable[T]] =
      createExpectable(values).applyMatcher(ContainWithResult(f).atLeast(n))

    def atLeast(n: Int)(f: T => R): MatchResult[Traversable[T]] =
      createExpectable(values).applyMatcher(ContainWithResult(f).atLeast(n))

    def atMost(n: Times)(f: T => R): MatchResult[Traversable[T]] =
      createExpectable(values).applyMatcher(ContainWithResult(f).atMost(n))

    def atMost(n: Int)(f: T => R): MatchResult[Traversable[T]] =
      createExpectable(values).applyMatcher(ContainWithResult(f).atMost(n))

    def between(min: Times, max: Times)(f: T => R): MatchResult[Traversable[T]] =
      createExpectable(values).applyMatcher(ContainWithResult(f).between(min, max))

    def between(min: Int, max: Int)(f: T => R): MatchResult[Traversable[T]] =
      createExpectable(values).applyMatcher(ContainWithResult(f).between(min, max))

    def exactly(n: Times)(f: T => R): MatchResult[Traversable[T]] =
      createExpectable(values).applyMatcher(ContainWithResult(f).exactly(n))

    def exactly(n: Int)(f: T => R): MatchResult[Traversable[T]] =
      createExpectable(values).applyMatcher(ContainWithResult(f).exactly(n))

object ResultImplicits extends ResultImplicits

trait SequenceMatchersCreation extends ExpectationsCreation with ResultImplicits { outer =>
  extension [T, A](f: T => Matcher[T]):

    /** @return a function which will return the composition of a matcher and a function */
    def ^^^(g: A => T): A => Matcher[A] = (a: A) =>
      new Matcher[A] {
        def apply[B <: A](b: Expectable[B]) =
          val originalValues = s"\nOriginal values\n  Expected: '$a'\n  Actual  : '${b.value}'"
          result(f(g(a)).apply(b.map(g)), b).updateMessage(_+originalValues)
      }

    def forall(values: Traversable[T])      = outer.forall     (values)((t: T) => f(t)(createExpectable(t)))
    def foreach(values: Traversable[T])     = outer.foreach    (values)((t: T) => f(t)(createExpectable(t)))
    def atLeastOnce(values: Traversable[T]) = outer.atLeastOnce(values)((t: T) => f(t)(createExpectable(t)))
    def atMostOnce(values: Traversable[T])  = outer.atMostOnce (values)((t: T) => f(t)(createExpectable(t)))

  /** verify the function f for all the values, stopping after the first failure */
  def forall[T, R : AsResult](values: Traversable[T])(f: T => R): MatchResult[Traversable[T]] =
    createExpectable(values).applyMatcher(ContainWithResult(f).forall)

  /** verify the function f for all the values, stopping after the first failure, where the PartialFunction is defined */
  def forallWhen[T, U](values: Traversable[T])(f: PartialFunction[T, MatchResult[U]]): MatchResult[Traversable[T]] =
    forall(values.filter(f.isDefinedAt))(f)

  /** verify the function f for all the values, and collect all failures */
  def foreach[T, R : AsResult](values: Traversable[T])(f: T => R): MatchResult[Traversable[T]] =
    createExpectable(values).applyMatcher(ContainWithResult(f).foreach)

  /** verify the function f for all the values, and collect all failures, where the PartialFunction is defined */
  def foreachWhen[T, R : AsResult](values: Traversable[T])(f: PartialFunction[T, R]): MatchResult[Traversable[T]] =
    foreach(values.filter(f.isDefinedAt))(f)

  /** verify the function f for at least one value */
  def atLeastOnce[T, R : AsResult](values: Traversable[T])(f: T => R): MatchResult[Traversable[T]] =
    values.atLeastOnce(f)

  /** verify the function f for at least one value */
  def atMostOnce[T, R : AsResult](values: Traversable[T])(f: T => R): MatchResult[Traversable[T]] =
    values.atMostOnce(f)

  /** verify the function f for at least one value, where the PartialFunction is defined */
  def atLeastOnceWhen[T, R : AsResult](values: Traversable[T])(f: PartialFunction[T, R]): MatchResult[Traversable[T]] =
    atLeastOnce(values.filter(f.isDefinedAt))(f)

  /** verify the function f for at least one value, where the PartialFunction is defined */
  def atMostOnceWhen[T, R : AsResult](values: Traversable[T])(f: PartialFunction[T, R]): MatchResult[Traversable[T]] =
    atMostOnce(values.filter(f.isDefinedAt))(f)
}

object SequenceMatchersCreation extends SequenceMatchersCreation

trait MatchersCreation:

  /**
   * This method transforms a function to a Matcher
   */
  given [T] as Conversion[(T => Boolean, String), Matcher[T]]:
    def apply(f: (T => Boolean, String)): Matcher[T] =
      (f._1, (t:T) => negateSentence(q(t)+" "+f._2), (t:T) => q(t)+" "+f._2)

  /**
   * This method transforms a function to a Matcher
   */
  given [T] as Conversion[(T => Boolean, String, String), Matcher[T]]:
    def apply(f: (T => Boolean, String, String)): Matcher[T] =
      (f._1, (t:T) => q(t)+" "+f._2, (t:T) => q(t)+" "+f._3)

  /**
   * This method transforms a function to a Matcher
   */
  given [T] as Conversion[(T => Boolean, T => String), Matcher[T]]:
    def apply(f: (T => Boolean, T => String)): Matcher[T] =
      (f._1, (t:T) => negateSentence(f._2(t)), f._2)

  /**
   * This method transforms a function, with function descriptors to a Matcher
   */
  given [T] as Conversion[(T => Boolean, T => String, T => String), Matcher[T]]:
   def apply(f: (T => Boolean, T => String, T => String)): Matcher[T] =
     new Matcher[T]:
       def apply[S <: T](s: Expectable[S]) =
         val functionResult = f._1(s.value)
         result(functionResult,  f._2(s.value), f._3(s.value), s)

  /**
   * This method transforms a function returning a pair (Boolean, String for ko message) to a Matcher
   */
  given pairFunctionToMatcher[T] as Conversion[T => (Boolean, String), Matcher[T]]:
   def apply(f: T => (Boolean, String)): Matcher[T] =
     new Matcher[T]:
       def apply[S <: T](s: Expectable[S]) =
         val functionResult = f(s.value)
         result(functionResult._1, negateSentence(functionResult._2), functionResult._2, s)

  /**
   * This method transforms a function returning a triplet (Boolean, String for ok message, String for ko message) to a Matcher
   */
  given [T] as Conversion[T => (Boolean, String, String), Matcher[T]]:
    def apply(f: T => (Boolean, String, String)): Matcher[T] =
      new Matcher[T]:
        def apply[S <: T](s: Expectable[S]) =
          val functionResult = f(s.value)
          result(functionResult._1, functionResult._2,  functionResult._3, s)

  /**
   * This method transforms a function returning a Result to a Matcher
   */
  given resultFunctionToMatcher[T, R : AsResult] as Conversion[T => R, Matcher[T]]:
    def apply(f: T => R): Matcher[T] =
      new Matcher[T]:
        def apply[S <: T](s: Expectable[S]) =
          val r = ResultExecution.execute(AsResult(f(s.value)))
          result(r, s)

  /** this allows a function returning a matcher to be used where the same function with a byname parameter is expected */
  given matcherFunctionToMatcher[T, R] as Conversion[T => Matcher[R], (=>T) => Matcher[R]]:
    def apply(f: T => Matcher[R]): (=>T) => Matcher[R] =
      def f1(t: =>T) = f(t)
      f1

  /**
   * this implicit provides an inverted syntax to adapt matchers to make the adaptation more readable in some cases:
   * - def haveExtension(extension: =>String) = ((_:File).getPath) ^^ endWith(extension)
   */
  extension [T, S](f: T => S)
    def ^^(m: Matcher[S]): Matcher[T] =
      m ^^ f


object MatchersCreation extends MatchersCreation
