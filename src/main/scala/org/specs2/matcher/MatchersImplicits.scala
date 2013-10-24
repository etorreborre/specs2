package org.specs2
package matcher

import execute._
import scalaz._, Scalaz._
import Foldable._
import collection.Seqx._
import Generator._
import text.Quote._
import text.Plural._
import MatchResultMessages._
import Result.ResultFailureMonoid
import scala.collection.{GenTraversable, GenTraversableOnce}
import ResultLogicalCombinators._
import text.Sentences
import control.Times

/**
* This trait provides implicit definitions from MatchResults and Booleans to Results.
*
* It also allows to:
*
* - create matchers from functions
* - create matchers for seqs and sets from single matchers
*/
trait MatchersImplicits extends Expectations with MatchResultCombinators with MatchResultImplicits with ExpectationsDescription { outer =>
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

    def forall(values: GenTraversableOnce[T])      = createExpectable(values).applyMatcher(cc.forall)
    def foreach(values: GenTraversableOnce[T])     = createExpectable(values).applyMatcher(cc.foreach)
    def atLeastOnce(values: GenTraversableOnce[T]) = createExpectable(values).applyMatcher(cc.atLeastOnce)
    def atMostOnce(values: GenTraversableOnce[T])  = createExpectable(values).applyMatcher(cc.atMostOnce)
  }

  implicit class matcherContainResult[T](m: Matcher[T]) { outer =>
    private val cc: ContainWithResult[T] = ContainWithResult(ValueChecks.matcherIsValueCheck(m))

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

    def forall(values: GenTraversableOnce[T])     : MatchResult[GenTraversableOnce[T]] = cc.forall     (createExpectable(values))
    def foreach(values: GenTraversableOnce[T])    : MatchResult[GenTraversableOnce[T]] = cc.foreach    (createExpectable(values))
    def atLeastOnce(values: GenTraversableOnce[T]): MatchResult[GenTraversableOnce[T]] = cc.atLeastOnce(createExpectable(values))
    def atMostOnce(values: GenTraversableOnce[T]) : MatchResult[GenTraversableOnce[T]] = cc.atMostOnce (createExpectable(values))
  }


  implicit class MatcherFunction[S, T](f: S => Matcher[T]) {
    /** @return a function which will return a matcher checking a sequence of objects  */
    def toSeq = (s: Seq[S]) => new SeqMatcher(s, f)

    /** @return a function which will return a matcher checking a set of objects */
    def toSet = (s: Set[S]) => new SetMatcher(s, f)

  }

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

    def forall(values: GenTraversable[T])      = outer.forall     (values)(applyMatcher)
    def foreach(values: GenTraversable[T])     = outer.foreach    (values)(applyMatcher)
    def atLeastOnce(values: GenTraversable[T]) = outer.atLeastOnce(values)(applyMatcher)
    def atMostOnce(values: GenTraversable[T])  = outer.atMostOnce (values)(applyMatcher)
  }

  /**
   * this implicit provides an inverted syntax to adapt matchers to make the adaptation more readable in some cases:
   * - def haveExtension(extension: =>String) = ((_:File).getPath) ^^ endWith(extension)
   */
  implicit class AdaptFunction[T, S](f: T => S) {
    def ^^(m: Matcher[S]): Matcher[T] = m ^^ f
  }

  /**
   * The `SeqMatcher` class is a matcher matching a sequence of objects with a matcher returned by a function.<p>
   * Usage: List(1, 2, 3) must ((beEqualTo(_:Int)).toSeq)(List(1, 2, 3))
   */
  class SeqMatcher[S, T](s: Seq[S], f: S => Matcher[T]) extends Matcher[Seq[T]] {
    def apply[U <: Seq[T]](t: Expectable[U]) = {
      val bothSequences = t.value.toList zip s.toList
      val results = bothSequences.map { case (t1, s1) => f(s1).apply(createExpectable(t1)) }
      if (s.size != t.value.size)
        result(false,
               "the seqs contain the same number of elements",
                t.description + " contains " + t.value.size + " elements while " + q(s) + " contains " + s.size + " elements",
                t)
      else
        result(FoldrGenerator[List].reduce(MatchResultMessageReducer[T], results), t)
    }
  }

  /**
   * The `SetMatcher` class is a matcher matching a set of objects with a matcher returned by a function.<p>
   * Usage: List(1, 2, 3) must ((beEqualTo(_:Int)).toSet)(List(2, 1, 3)) }}}
   */
  class SetMatcher[S, T](s: Set[S], f: S => Matcher[T]) extends Matcher[Set[T]] {
    def apply[U <: Set[T]](t: Expectable[U]) = {
      val setToTest = t
      if (s.size != setToTest.value.size)
        result(false, 
               "the sets contain the same number of elements",
                setToTest.description + " contains " + setToTest.value.size + " elements while " + q(s) + " contains " + s.size + " elements",
                setToTest)
      else {
        val results = setToTest.value.map { (element: T) =>
          s.find { (otherElement:S) => f(otherElement).apply(createExpectable(element)).isSuccess } match {
            case None => result(false, "all matches", "no match for element " + q(element), setToTest)
            case Some(x) => result(true, q(element) + " matches with " + x, "no match for element " + q(element), setToTest)
          }
        }
        result(FoldrGenerator[Set].reduce(MatchResultMessageReducer[U], results), setToTest)
      }
    }
  }

  /** verify the function f for all the values, stopping after the first failure */
  def forall[T, R : AsResult](values: GenTraversableOnce[T])(f: T => R) = f.forall(values)
  /** apply a matcher for all values */
  def forall[T](matcher: Matcher[T]) = matcher.forall
  /** verify the function f for all the values, stopping after the first failure, where the PartialFunction is defined */
  def forallWhen[T, U](values: GenTraversable[T])(f: PartialFunction[T, MatchResult[U]]) = forall(values.filter(f.isDefinedAt))(f)
  /** verify the function f for all the values, and collect all failures */
  def foreach[T, R : AsResult](values: GenTraversableOnce[T])(f: T => R) = f.foreach(values)
  /** apply a matcher foreach value */
  def foreach[T](matcher: Matcher[T]) = matcher.foreach
  /** verify the function f for all the values, and collect all failures, where the PartialFunction is defined */
  def foreachWhen[T, R : AsResult](values: GenTraversable[T])(f: PartialFunction[T, R]) = foreach(values.filter(f.isDefinedAt))(f)
  /** verify the function f for at least one value */
  def atLeastOnce[T, R : AsResult](values: GenTraversableOnce[T])(f: T => R) = f.atLeastOnce(values)
  /** verify the function f for at least one value */
  def atMostOnce[T, R : AsResult](values: GenTraversableOnce[T])(f: T => R) = f.atMostOnce(values)
  /** apply a matcher atLeast one value */
  def atLeastOnce[T](matcher: Matcher[T]) = matcher.atLeastOnce
  /** apply a matcher atLeast one value */
  def atMostOnce[T](matcher: Matcher[T]) = matcher.atMostOnce
  /** verify the function f for at least one value, where the PartialFunction is defined */
  def atLeastOnceWhen[T, R : AsResult](values: GenTraversable[T])(f: PartialFunction[T, R]) = atLeastOnce(values.filter(f.isDefinedAt))(f)
  /** verify the function f for at least one value, where the PartialFunction is defined */
  def atMostOnceWhen[T, R : AsResult](values: GenTraversable[T])(f: PartialFunction[T, R]) = atMostOnce(values.filter(f.isDefinedAt))(f)
  /**
   * This method transform a function to a Matcher
   */
  implicit def functionToMatcher[T](f: (T => Boolean, String)): Matcher[T] =
    functionAndMessagesToMatcher[T]((f._1, (t:T) => negateSentence(q(t)+" "+f._2), (t:T) => q(t)+" "+f._2))
  /**
   * This method transform a function to a Matcher
   */
  implicit def functionToMatcher2[T](f: (T => Boolean, String, String)): Matcher[T] =
    functionAndMessagesToMatcher[T]((f._1, (t:T) => q(t)+" "+f._2, (t:T) => q(t)+" "+f._3))
  /**
   * This method transform a function to a Matcher
   */
  implicit def functionAndKoMessageToMatcher[T](f: (T => Boolean, T => String)): Matcher[T] =
    functionAndMessagesToMatcher[T]((f._1, (t:T) => negateSentence(f._2(t)), f._2))
  /**
   * This method transform a function, with function descriptors to a Matcher
   */
  implicit def functionAndMessagesToMatcher[T](f: (T => Boolean, T => String, T => String)): Matcher[T] = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]) = {
      val functionResult = f._1(s.value)
      result(functionResult,  f._2(s.value), f._3(s.value), s)
    }
  }
  /**
   * This method transform a function returning a pair (Boolean, String for ko message) to a Matcher
   */
  implicit def pairFunctionToMatcher[T](f: T =>(Boolean, String)): Matcher[T] = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]) = {
      val functionResult = f(s.value)
      result(functionResult._1, negateSentence(functionResult._2), functionResult._2, s)
    }
  }
  /**
   * This method transform a function returning a triplet (Boolean, String for ok message, String for ko message) to a Matcher
   */
  implicit def tripletFunctionToMatcher[T](f: T =>(Boolean, String, String)): Matcher[T] = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]) = {
      val functionResult = f(s.value)
      result(functionResult._1, functionResult._2,  functionResult._3, s)
    }
  }
  /**
   * This method transform a function returning a Result to a Matcher
   */
  implicit def matchResultFunctionToMatcher[T, R : AsResult](f: T => R): Matcher[T] = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]) = {
      val r = ResultExecution.execute(AsResult(f(s.value)))
      result(r.isSuccess, r.message, r.message, s)
    }
  }

}

private[specs2]
object MatchersImplicits extends MatchersImplicits

/**
 * Implicit conversions for MatchResults
 */
private[specs2]
trait MatchResultImplicits { outer =>
  /**
  * implicit definition to transform a Seq of MatchResults to a Result
  */
  implicit def seqToResult[T](r: Seq[MatchResult[T]]): Result = r.foldLeft(StandardResults.success: Result)(_ and _.toResult)

  /**
   * implicit definition to transform any MatchResult to a Result
   */
  implicit def asResult[M[_] <: MatchResult[_]](r: M[_]): Result = ResultExecution.execute(r.toResult)

  /**
   * implicit definition to accept any MatchResult as a Boolean value.
   * It is true if the MatchResult is not an Error or a Failure
   */
  implicit def fromMatchResult(r: =>MatchResult[_]): Boolean = r.isSuccess || r.toResult.isSkipped || r.toResult.isPending

  /** implicit typeclass instance to create examples from MatchResults */
  implicit def matchResultAsResult[M[_] <: MatchResult[_]]: AsResult[M[_]] = new AsResult[M[_]] {
    def asResult(t: =>M[_]): Result = outer.asResult(t)
  }

  /** implicit typeclass instance to create examples from a sequence of MatchResults */
  implicit def matchResultSeqAsResult[T]: AsResult[Seq[MatchResult[T]]] = new AsResult[Seq[MatchResult[T]]] {
    def asResult(t: =>Seq[MatchResult[T]]): Result = t.foldLeft(StandardResults.success: Result)(_ and _.toResult)
  }
}
