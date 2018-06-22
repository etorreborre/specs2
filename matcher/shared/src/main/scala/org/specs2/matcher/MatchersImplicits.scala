package org.specs2
package matcher

import execute._
import text.Quote._
import scala.collection.Traversable
import ResultLogicalCombinators._
import text.Sentences._
import control.Times

/**
* This trait provides implicit definitions from MatchResults and Booleans to Results.
*
* It also allows to:
*
* - create matchers from functions
* - create matchers for seqs and sets from single matchers
*/
trait MatchersImplicits extends
       MatchResultCombinators
  with MatcherZipOperators
  with MatchResultImplicits
  with ResultImplicits
  with MatchersCreation
  with SequenceMatchersCreation

object MatchersImplicits extends MatchersImplicits

/**
 * Implicit conversions for MatchResults
 */
trait MatchResultImplicits { outer =>
  /**
   * implicit definition to transform a Seq of MatchResults to a Result
   */
  implicit def seqToResult[T](r: Seq[MatchResult[T]]): Result = r.foldLeft(StandardResults.success: Result)(_ and _.toResult)

  /**
   * implicit definition to transform any MatchResult to a Result
   */
  implicit def asResult[T](r: MatchResult[T]): Result = ResultExecution.execute(r.toResult)

  /**
   * implicit definition to accept any MatchResult as a Boolean value.
   * It is true if the MatchResult is not an Error or a Failure
   */
  implicit def fromMatchResult(r: =>MatchResult[_]): Boolean = r.isSuccess || r.toResult.isSkipped || r.toResult.isPending

}

object MatchResultImplicits extends MatchResultImplicits

trait ResultImplicits extends ExpectationsCreation {
  /**
   * Add functionalities to functions returning matchers so that they can be combined before taking a value and
   * returning actual matchers
   */
  implicit class resultFunction[T, R : AsResult](f: T => R) {
    private val cc: ContainWithResult[T] = ContainWithResult(ValueChecks.functionIsValueCheck(f))

    def forall                          : ContainWithResult[T] = cc.forall
    def foreach                         : ContainWithResult[T] = cc.foreach
    def atLeastOnce                     : ContainWithResult[T] = cc.atLeastOnce
    def atMostOnce                      : ContainWithResult[T] = cc.atMostOnce
    def atLeast(n: Times)               : ContainWithResult[T] = cc.atLeast(n)
    def atLeast(n: Int)                 : ContainWithResult[T] = cc.atLeast(n)
    def atMost(n: Times)                : ContainWithResult[T] = cc.atMost(n)
    def atMost(n: Int)                  : ContainWithResult[T] = cc.atMost(n)
    def between(min: Times, max: Times) : ContainWithResult[T] = cc.between(min, max)
    def between(min: Int, max: Int)     : ContainWithResult[T] = cc.between(min, max)
    def exactly(n: Times)               : ContainWithResult[T] = cc.exactly(n)
    def exactly(n: Int)                 : ContainWithResult[T] = cc.exactly(n)

    def forall(values: Traversable[T])      = createExpectable(values).applyMatcher(cc.forall)
    def foreach(values: Traversable[T])     = createExpectable(values).applyMatcher(cc.foreach)
    def atLeastOnce(values: Traversable[T]) = createExpectable(values).applyMatcher(cc.atLeastOnce)
    def atMostOnce(values: Traversable[T])  = createExpectable(values).applyMatcher(cc.atMostOnce)
  }
}

object ResultImplicits extends ResultImplicits

trait SequenceMatchersCreation extends ExpectationsCreation with ResultImplicits { outer =>
  implicit class InvariantMatcherFunction[T](f: T => Matcher[T]) {
    /** @return a function which will return the composition of a matcher and a function */
    def ^^^[A](g: A => T) = (a: A) =>
      new Matcher[A] {
        def apply[B <: A](b: Expectable[B]) = {
          val originalValues = s"\nOriginal values\n  Expected: '$a'\n  Actual  : '${b.value}'"
          result(f(g(a)).apply(b.map(g)), b).updateMessage(_+originalValues)
        }
      }

    private def applyMatcher = (t: T) => f(t)(createExpectable(t))

    def forall(values: Traversable[T])      = outer.forall     (values)(applyMatcher)
    def foreach(values: Traversable[T])     = outer.foreach    (values)(applyMatcher)
    def atLeastOnce(values: Traversable[T]) = outer.atLeastOnce(values)(applyMatcher)
    def atMostOnce(values: Traversable[T])  = outer.atMostOnce (values)(applyMatcher)
  }

  /** verify the function f for all the values, stopping after the first failure */
  def forall[T, R : AsResult](values: Traversable[T])(f: T => R) = f.forall(values)
  /** apply a matcher for all values */
  def forall[T](matcher: Matcher[T]) = ContainWithResult(ValueChecks.matcherIsValueCheck(matcher)).forall
  /** verify the function f for all the values, stopping after the first failure, where the PartialFunction is defined */
  def forallWhen[T, U](values: Traversable[T])(f: PartialFunction[T, MatchResult[U]]) = forall(values.filter(f.isDefinedAt))(f)
  /** verify the function f for all the values, and collect all failures */
  def foreach[T, R : AsResult](values: Traversable[T])(f: T => R) = f.foreach(values)
  /** apply a matcher foreach value */
  def foreach[T](matcher: Matcher[T]) = ContainWithResult(ValueChecks.matcherIsValueCheck(matcher)).foreach
  /** verify the function f for all the values, and collect all failures, where the PartialFunction is defined */
  def foreachWhen[T, R : AsResult](values: Traversable[T])(f: PartialFunction[T, R]) = foreach(values.filter(f.isDefinedAt))(f)
  /** verify the function f for at least one value */
  def atLeastOnce[T, R : AsResult](values: Traversable[T])(f: T => R) = f.atLeastOnce(values)
  /** verify the function f for at least one value */
  def atMostOnce[T, R : AsResult](values: Traversable[T])(f: T => R) = f.atMostOnce(values)
  /** apply a matcher atLeast one value */
  def atLeastOnce[T](matcher: Matcher[T]) = ContainWithResult(ValueChecks.matcherIsValueCheck(matcher)).atLeastOnce
  /** apply a matcher atLeast one value */
  def atMostOnce[T](matcher: Matcher[T]) = ContainWithResult(ValueChecks.matcherIsValueCheck(matcher)).atMostOnce
  /** verify the function f for at least one value, where the PartialFunction is defined */
  def atLeastOnceWhen[T, R : AsResult](values: Traversable[T])(f: PartialFunction[T, R]) = atLeastOnce(values.filter(f.isDefinedAt))(f)
  /** verify the function f for at least one value, where the PartialFunction is defined */
  def atMostOnceWhen[T, R : AsResult](values: Traversable[T])(f: PartialFunction[T, R]) = atMostOnce(values.filter(f.isDefinedAt))(f)
}

object SequenceMatchersCreation extends SequenceMatchersCreation

trait MatchersCreation {

  /**
   * This method transforms a function to a Matcher
   */
  implicit def functionToMatcher[T](f: (T => Boolean, String)): Matcher[T] =
    functionAndMessagesToMatcher[T]((f._1, (t:T) => negateSentence(q(t)+" "+f._2), (t:T) => q(t)+" "+f._2))

  /**
   * This method transforms a function to a Matcher
   */
  implicit def functionToMatcher2[T](f: (T => Boolean, String, String)): Matcher[T] =
    functionAndMessagesToMatcher[T]((f._1, (t:T) => q(t)+" "+f._2, (t:T) => q(t)+" "+f._3))
  /**
   * This method transforms a function to a Matcher
   */
  implicit def functionAndKoMessageToMatcher[T](f: (T => Boolean, T => String)): Matcher[T] =
    functionAndMessagesToMatcher[T]((f._1, (t:T) => negateSentence(f._2(t)), f._2))
  /**
   * This method transforms a function, with function descriptors to a Matcher
   */
  implicit def functionAndMessagesToMatcher[T](f: (T => Boolean, T => String, T => String)): Matcher[T] = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]) = {
      val functionResult = f._1(s.value)
      result(functionResult,  f._2(s.value), f._3(s.value), s)
    }
  }
  /**
   * This method transforms a function returning a pair (Boolean, String for ko message) to a Matcher
   */
  implicit def pairFunctionToMatcher[T](f: T =>(Boolean, String)): Matcher[T] = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]) = {
      val functionResult = f(s.value)
      result(functionResult._1, negateSentence(functionResult._2), functionResult._2, s)
    }
  }
  /**
   * This method transforms a function returning a triplet (Boolean, String for ok message, String for ko message) to a Matcher
   */
  implicit def tripletFunctionToMatcher[T](f: T =>(Boolean, String, String)): Matcher[T] = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]) = {
      val functionResult = f(s.value)
      result(functionResult._1, functionResult._2,  functionResult._3, s)
    }
  }
  /**
   * This method transforms a function returning a Result to a Matcher
   */
  implicit def matchResultFunctionToMatcher[T, R : AsResult](f: T => R): Matcher[T] = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]) = {
      val r = ResultExecution.execute(AsResult(f(s.value)))
      result(r, s)
    }
  }

  /** this allows a function returning a matcher to be used where the same function with a byname parameter is expected */
  implicit def stringMatcherFunctionToBynameMatcherFunction[T, R](f: T => Matcher[R]): (=>T) => Matcher[R] = {
    def f1(t: =>T) = f(t)
    f1
  }

  /**
   * this implicit provides an inverted syntax to adapt matchers to make the adaptation more readable in some cases:
   * - def haveExtension(extension: =>String) = ((_:File).getPath) ^^ endWith(extension)
   */
  implicit class AdaptFunction[T, S](f: T => S) {
    def ^^(m: Matcher[S]): Matcher[T] = m ^^ f
  }

}

object MatchersCreation extends MatchersCreation
