package org.specs2
package matcher

import execute._
import org.specs2.internal.scalaz._, Scalaz._
import Foldable._
import Generator._
import text.Quote._
import text.Plural._
import MatchResultMessages._
import Result.ResultFailureMonoid
import scala.collection.{GenTraversable, GenTraversableOnce}

/**
* This trait provides implicit definitions from MatchResults and Booleans to Results.
*
* It also allows to:
*
* - create matchers from functions
* - create matchers for seqs and sets from single matchers
*/
trait MatchersImplicits extends Expectations {
  /** 
   * implicit definition to transform a Seq of MatchResults to a Result
   */ 
  implicit def seqToResult[T](r: Seq[MatchResult[T]]): Result = r.reduceLeft(_ and _).toResult
  /** 
   * implicit definition to transform any MatchResult to a Result
   */ 
  implicit def asResult[T](r: MatchResult[T]): Result = r.toResult
  
  /** 
   * implicit definition to accept any boolean value as a Result
   * This avoids writing b must beTrue 
   */ 
  implicit def toResult(b: Boolean): Result = {
    new BeTrueMatcher().apply(createExpectable(b)).toResult
  }

  /**
   * implicit definition to accept any MatchResult as a Boolean value.
   * It is true if the MatchResult is not an Error or a Failure
   */
  implicit def fromMatchResult(r: =>MatchResult[_]): Boolean = r.isSuccess || r.isSkipped || r.isPending
  /**
   * Add functionalities to functions returning matchers so that they can be combined before taking a value and
   * returning actual matchers
   */
  implicit def matcherFunction[S, T](f: S => Matcher[T]) = new MatcherFunction(f)
  implicit def matcherFunction2[T](f: T => Matcher[T]) = new MatcherFunction2(f)

  class MatcherFunction[S, T](f: S => Matcher[T]) {
  
    /**
     * @return a function which will return a matcher checking a sequence of objects
     */
    def toSeq = new Function1[Seq[S], Matcher[Seq[T]]] {
      def apply(s: Seq[S]) = new SeqMatcher(s, f)
    }

    /**
     * @return a function which will return a matcher checking a set of objects
     */
    def toSet = new Function1[Set[S], Matcher[Set[T]]] {
      def apply(s: Set[S]) = new SetMatcher(s, f)
    }

  }

  class MatcherFunction2[T](f: T => Matcher[T]) {
    /**
     * @return a function which will return the composition of a matcher and a function
     */
    def ^^^[A](g: A => T) = (a: A) => 
      new Matcher[A] {
        def apply[B <: A](b: Expectable[B]) = {
          val r = f(g(a)).apply(b.map(g))
          result(r, b)
        }
      }

    /**
     * check that the function is valid for all value, stopping after the first failure
     */
    def forall(values: Seq[T]): MatchResult[Seq[T]] = verifyFunction((t: T) => f(t).apply(Expectable(t))).forall(values)
    /**
     * check that the function is valid for each value, showing all the failures
     */
    def foreach(values: Seq[T]): MatchResult[Seq[T]] = verifyFunction((t: T) => f(t).apply(Expectable(t))).foreach(values)
    /**
     * check that the function is valid at least once
     */
    def atLeastOnce(values: Seq[T]): MatchResult[Seq[T]] = verifyFunction((t: T) => f(t).apply(Expectable(t))).atLeastOnce(values)
  }

  /**
   * The <code>SeqMatcher</code> class is a matcher matching a sequence of objects with a matcher returned by a function.<p>
   * Usage:<code>List(1, 2, 3) must ((beEqualTo(_:Int)).toSeq)(List(1, 2, 3)) </code>
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
   * The <code>SetMatcher</code> class is a matcher matching a set of objects with a matcher returned by a function.<p>
   * Usage:<code>List(1, 2, 3) must ((beEqualTo(_:Int)).toSet)(List(2, 1, 3)) </code>
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
  def forall[T, U](values: GenTraversableOnce[T])(f: T => MatchResult[U])      = verifyFunction(f).forall(values.seq.toSeq)
  /** verify the function f for all the values, stopping after the first failure, where the PartialFunction is defined */
  def forallWhen[T, U](values: GenTraversable[T])(f: PartialFunction[T, MatchResult[U]]) = forall(values.filter(f.isDefinedAt))(f)
  /** verify the function f for all the values, and collect all failures */
  def foreach[T, U](values: GenTraversableOnce[T])(f: T => MatchResult[U])     = verifyFunction(f).foreach(values.seq.toSeq)
  /** verify the function f for all the values, and collect all failures, where the PartialFunction is defined */
  def foreachWhen[T, U](values: GenTraversable[T])(f: PartialFunction[T, MatchResult[U]]) = foreach(values.filter(f.isDefinedAt))(f)
  /** verify the function f for at least one value */
  def atLeastOnce[T, U](values: GenTraversableOnce[T])(f: T => MatchResult[U]) = verifyFunction(f).atLeastOnce(values.seq.toSeq)
  /** verify the function f for at least one value, where the PartialFunction is defined */
  def atLeastOnceWhen[T, U](values: GenTraversable[T])(f: PartialFunction[T, MatchResult[U]]) = atLeastOnce(values.filter(f.isDefinedAt))(f)
  /**
   * This method transform a function to a Matcher
   */
  implicit def functionToMatcher[T](f: (T => Boolean, String)): Matcher[T] =
    functionAndMessagesToMatcher[T]((f._1, (t:T) => "not ("+q(t)+" "+f._2+")", (t:T) => q(t)+" "+f._2))
  /**
   * This method transform a function to a Matcher
   */
  implicit def functionToMatcher2[T](f: (T => Boolean, String, String)): Matcher[T] =
    functionAndMessagesToMatcher[T]((f._1, (t:T) => q(t)+" "+f._2, (t:T) => q(t)+" "+f._3))
  /**
   * This method transform a function to a Matcher
   */
  implicit def functionAndKoMessageToMatcher[T](f: (T => Boolean, T => String)): Matcher[T] =
    functionAndMessagesToMatcher[T]((f._1, (t:T) => "not ("+f._2(t)+")", f._2))
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
      result(functionResult._1, "not ("+functionResult._2+")", functionResult._2, s)
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
   * This method transform a function returning a MatchResult to a Matcher
   */
  implicit def matchResultFunctionToMatcher[T](f: T => MatchResult[_]): Matcher[T] = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]) = {
      val functionResult = f(s.value)
      result(functionResult.isSuccess, functionResult.message, functionResult.message, s)
    }
  }

  implicit def verifyFunction[U, T](t: U => MatchResult[T]) = new MatchResultFunctionVerification(t)
  class MatchResultFunctionVerification[U, T](function: U => MatchResult[T]) {
    def forall[S <: Traversable[U]](seq: S) = {
      val expectable = createExpectable(seq)
      val result =
        if (seq.isEmpty)
          Matcher.result(true, "ok", "ko", expectable)
        else {
          val (r, lastValueTried) = seq.drop(1).foldLeft(executeFunctionAndReturnValue(seq.head)) { (res, cur) =>
            if (res._1.isSuccess) executeFunctionAndReturnValue(cur)
            else                  res
          }
          lazy val failingElementIndex = if (r.isSuccess) -1 else seq.toSeq.indexOf(lastValueTried)
          lazy val failingElementMessage =
            if (failingElementIndex >= 0)
              "In the sequence "+q(seq.mkString(", "))+", the "+(failingElementIndex+1).th+" element is failing: "+r.message
            else
              r.message

          makeSeqResult(r, "All elements of "+q(seq.mkString(", "))+" are matching ok", failingElementMessage, expectable)
        }
      checkFailure(result)
    }

    def foreach[S <: Traversable[U]](seq: S) = {
      val expectable = createExpectable(seq)
      val result =
        if (seq.isEmpty)
          Matcher.result(true, "ok", "ko", expectable)
        else {
          val r = seq.toSeq.foldMap(executeFunction(_))
          makeSeqResult(r, "All elements of "+q(seq.mkString(", "))+" are successful", r.message, expectable)
        }
      checkFailure(result)
    }

    def atLeastOnce[S <: Traversable[U]](seq: S) = {
      val expectable = createExpectable(seq)
      val result =
        if (seq.isEmpty)
          Matcher.result(false, "ok", "ko", expectable)
        else {
          val (r, lastTriedValue) = seq.drop(1).foldLeft(executeFunctionAndReturnValue(seq.head)) { (res, cur) =>
            if (res._1.isSuccess) res
            else                  executeFunctionAndReturnValue(cur)
          }
          makeSeqResult(r, "In the sequence "+q(seq.mkString(", "))+
                           ", the "+(seq.toSeq.indexOf(lastTriedValue)+1).th+" element is matching: "+r.message,
                           "No element of "+q(seq.mkString(", "))+" is matching ok",
                           expectable)
        }
      checkFailure(result)
    }

    private def executeFunctionAndReturnValue(value: U): (Result, U) = (executeFunction(value), value)
    private def executeFunction(value: U): Result = ResultExecution.execute(function(value).toResult)

    private def makeSeqResult[T](r: Result, okMessage: String, koMessage: String, expectable: Expectable[T]): MatchResult[T] =
      if (r.isSkipped)      MatchSkip(r.message, expectable)
      else if (r.isPending) MatchPending(r.message, expectable)
      else                  Matcher.result(r.isSuccess, okMessage, koMessage, expectable)

  }
}
private[specs2]
object MatchersImplicits extends MatchersImplicits
