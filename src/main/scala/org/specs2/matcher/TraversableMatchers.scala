package org.specs2
package matcher

import control._
import data.Sized
import text.Quote._
import text.Plural._
import collection.Iterablex._
import collection.Seqx._
import MatchersImplicits._
import scala.collection.{GenSeq, GenTraversableOnce, GenTraversable}
import execute._
import control.Times

/**
 * Matchers for traversables
 */
trait TraversableMatchers extends TraversableBaseMatchers with TraversableBeHaveMatchers
object TraversableMatchers extends TraversableMatchers

private[specs2]
trait TraversableBaseMatchersLowImplicits { this: TraversableBaseMatchers =>

  /** this allows the contain(string) matcher for StringMatchers to be used with a Traversable */
  implicit def stringMatcherIsTraversableMatcher(m: Matcher[String]): Matcher[GenTraversableOnce[String]] =
    containWithMatcherIsTraversableMatcher(contain(matcherIsContainCheck(m)))
}

private[specs2]
trait TraversableBaseMatchers extends LazyParameters with ImplicitParameters with TraversableBaseMatchersLowImplicits { outer =>
  
  trait TraversableMatcher[T] extends Matcher[GenTraversableOnce[T]]
  private type IP = ImplicitParam

  implicit def matcherIsContainCheck[T](m: Matcher[T]): ContainCheck[T] = new ContainCheck[T] {
    def check    = (t: T) => AsResult(m(Expectable(t)))
    def checkNot = (t: T) => AsResult(m.not(Expectable(t)))
    def messages(expectable: String, successes: Seq[Result], failures: Seq[Result]) =
      ContainCheck.genericMessages(expectable, successes, failures)
  }

  implicit def valueIsTypedContainCheck[T](expected: T): ContainCheck[T] = new ContainCheck[T] {
    private lazy val matcher = new BeTypedEqualTo(expected)
    def check    = (t: T) => AsResult(matcher(Expectable(t)))
    def checkNot = (t: T) => AsResult(matcher.not(Expectable(t)))
    def messages(expectable: String, successes: Seq[Result], failures: Seq[Result]) =
      (s"$expectable contains $expected", s"$expectable does not contain $expected")
  }

  implicit def functionIsContainCheck[T, R : AsResult](f: T => R): ContainCheck[T] = new ContainCheck[T] {
    def check    = (t: T) => AsResult(f(t))
    def checkNot = (t: T) => AsResult(f(t)).not
    def messages(expectable: String, successes: Seq[Result], failures: Seq[Result]) =
      ContainCheck.genericMessages(expectable, successes, failures)
  }

  implicit def containWithMatcherIsTraversableMatcher[T, U <: T](cm: ContainWithResult[T]): Matcher[GenTraversableOnce[U]] = new Matcher[GenTraversableOnce[U]] {
    def apply[S <: GenTraversableOnce[U]](t: Expectable[S]) = cm(t)
  }

  def contain[T](check: ContainCheck[T]): ContainWithResult[T] = new ContainWithResult(check)

  def contain[T](cm: ContainWithMatcherSeq[T]): ContainWithMatcherSeq[T] = cm

  def exactly[T](checks: ContainCheck[T]*) : ContainWithMatcherSeq[T] = new ContainWithMatcherSeq(checks).exactly
  def allOf[T](checks: ContainCheck[T]*)   : ContainWithMatcherSeq[T] = new ContainWithMatcherSeq(checks).atLeast
  def atLeast[T](checks: ContainCheck[T]*) : ContainWithMatcherSeq[T] = new ContainWithMatcherSeq(checks).atLeast
  def atMost[T](checks: ContainCheck[T]*)  : ContainWithMatcherSeq[T] = new ContainWithMatcherSeq(checks).atMost

  /** match if a traversable contains all the elements of seq (and maybe more) */
  def containAllOf[T](seq: Seq[T]) = contain(atLeast(seq:_*))
  /** match if a traversable contains one of (t1, t2) */
  def containAnyOf[T](seq: Seq[T]) = if (seq.isEmpty) beEmpty else contain(atLeast(seq.reduceLeft(_ or _)))

  /** match if traversable contains (x matches p) */
  def containPattern[T](t: =>String) = containMatch[T](".*"+t+".*")
  /** match if traversable contains (x matches .*+t+.*) */
  def containMatch[T](t: =>String) = contain(new BeMatching(t))

  /** does a containAll comparison in both ways */
  def containTheSameElementsAs[T](seq: Seq[T]): Matcher[Traversable[T]] = new Matcher[Traversable[T]] {

    def apply[S <: Traversable[T]](t: Expectable[S]) = {
      val missing = (seq.toSeq.diff(t.value.toSeq))
      val added   = (t.value.toSeq.diff(seq.toSeq))
      def message(diffs: Seq[_], msg: String) =
        if (diffs.isEmpty) "" else diffs.mkString("\n  "+msg+": ", ", ", "")

      result(missing.isEmpty && added.isEmpty,
             t.value + "\n  contains the same elements as\n"+ seq,
             t.value + message(missing, "is missing") + message(added, "must not contain"),
             t)
    }
  }

  /**
   * DEPRECATED MATCHERS
   */

  /**
   * Matches if there is one element in the traversable verifying the `function` parameter: `(traversable.exists(function(_))`
   * @deprecated(message="use contain(function) instead", since="2.0")
   */
  def have[T](function: T => Boolean) = new Matcher[GenTraversableOnce[T]]{
    def apply[S <: GenTraversableOnce[T]](traversable: Expectable[S]) = {
      result(traversable.value.exists(function(_)),
             "at least one element verifies the property in " + traversable.description, 
             "no element verifies the property in " + traversable.description,
             traversable)
    }
  }

  /**
   * Matches if there is at least one matching a "like" function
   * @deprecated(message="use contain(like(partialFunction))" instead, since="2.0")
   */
  def haveOneElementLike[T, S](like: PartialFunction[T, MatchResult[S]]) = HaveOneElementLike(like)
  /**
   * Matches if there is at least one matching a "like" function
   * @deprecated(message="use contain(like(partialFunction))" instead, since="2.0")
   */
  def oneElementLike[T, S](like: PartialFunction[T, MatchResult[S]]) = haveOneElementLike(like)

  /**
   * Matches if all elements are matching a "like" function
   * @deprecated(message="use contain(like(partialFunction)).forall" instead, since="2.0")
   */
  def haveAllElementsLike[T, S](like: PartialFunction[T, MatchResult[S]]) = HaveAllElementsLike(like)
  /**
   * Matches if all elements are matching a "like" function
   * @deprecated(message="use contain(like(partialFunction)).forall" instead, since="2.0")
   */
  def allElementsLike[T, S](like: PartialFunction[T, MatchResult[S]]) = haveAllElementsLike(like)

  /**
   * Matches if there l contains the same elements as the Traversable `traversable`.<br>
   * @deprecated(message="use contain(atLeast(seq.mappartialFunction)).forall" instead, since="2.0")
   */
  def haveTheSameElementsAs[T](l: =>Traversable[T], equality: (T, T) => Boolean = (_:T) == (_:T)) =
    new HaveTheSameElementsAs(l.toSeq, equality)

  private def containLike[T](pattern: =>String, matchType: String) =
    new ContainLikeMatcher[T](pattern, matchType) 

  /** match if there is a way to size T */
  def haveSize[T : Sized](n: Int) = new SizedMatcher[T](n, "size")
  /** alias for haveSize */
  def size[T : Sized](n: Int) = haveSize[T](n)
  /** alias for haveSize */
  def haveLength[T : Sized](n: Int) = new SizedMatcher[T](n, "length")
  /** alias for haveSize */
  def length[T : Sized](n: Int) = haveLength[T](n)

  /** @return a matcher checking if the elements are ordered */
  def beSorted[T : Ordering] = new OrderingMatcher[T]
  /** alias for beSorted */
  def sorted[T : Ordering] = beSorted[T]

  /** any scala collection has a size */
  implicit def scalaTraversableIsSized[I <: GenTraversable[_]]: Sized[I] = new Sized[I] {
    def size(t: I) = t.size
  }
  /** any scala array has a size */
  implicit def scalaArrayIsSized[T]: Sized[Array[T]] = new Sized[Array[T]] {
    def size(t: Array[T]) = t.length
  }
  /** any java collection has a size */
  implicit def javaCollectionIsSized[T <: java.util.Collection[_]]: Sized[T] = new Sized[T] {
    def size(t: T) = t.size()
  }
  /** a regular string has a size, without having to be converted to an Traversable */
  implicit def stringIsSized: Sized[String] = new Sized[String] {
    def size(t: String) = t.size
  }

}

private[specs2]
trait TraversableBeHaveMatchers extends LazyParameters { outer: TraversableMatchers =>

  implicit def traversable[T](s: MatchResult[GenTraversableOnce[T]]) = new TraversableBeHaveMatchers(s)
  class TraversableBeHaveMatchers[T](s: MatchResult[GenTraversableOnce[T]]) {
    def contain(check: ContainCheck[T]) = s match {
      case NotMatch(r) => r.expectable.applyMatcher(containWithMatcherIsTraversableMatcher(outer.contain(check.not)))
      case other       => s.expectable.applyMatcher(containWithMatcherIsTraversableMatcher(outer.contain(check)))
    }
    def containMatch(t: =>String) = s(outer.containMatch(t))
    def containPattern(t: =>String) = s(outer.containPattern(t))
    def have(f: T => Boolean) = s(outer.have(f))
    def oneElementLike[U](like: PartialFunction[T, MatchResult[U]]) = s(outer.haveOneElementLike(like))
    def allElementsLike[U](like: PartialFunction[T, MatchResult[U]]) = s(outer.haveAllElementsLike(like))
  }

  implicit def sized[T : Sized](s: MatchResult[T]) = new HasSize(s)
  class HasSize[T : Sized](s: MatchResult[T]) {
    def size(n: Int) : MatchResult[T] = s(outer.haveSize[T](n))
    def length(n: Int) : MatchResult[T] = size(n)
  }

  implicit def orderedSeqMatchResult[T : Ordering](result: MatchResult[Seq[T]]) = new OrderedSeqMatchResult(result)
  class OrderedSeqMatchResult[T : Ordering](result: MatchResult[Seq[T]]) {
    def sorted = result(outer.beSorted[T])
    def beSorted = result(outer.beSorted[T])
  }

}
class ContainMatchResult[T] private[specs2](val s: MatchResult[GenTraversableOnce[T]], containMatcher: ContainMatcher[T]) extends AbstractContainMatchResult[T] { outer =>
  val matcher = containMatcher
  def only = new ContainOnlyMatchResult(s, containMatcher.only)
  def inOrder = new ContainInOrderMatchResult(s, containMatcher.inOrder)
}
class ContainNotMatchResult[T] private[specs2](override val s: MatchResult[GenTraversableOnce[T]], containMatcher: ContainMatcher[T]) extends ContainMatchResult[T](s, containMatcher) { outer =>
  override val matcher = containMatcher
  override lazy val matchResult = s(matcher.not)
  override def only = new ContainOnlyMatchResult(s, containMatcher.only.not)
  override def inOrder = new ContainInOrderMatchResult(s, containMatcher.inOrder.not)
}
class ContainOnlyMatchResult[T] private[specs2](val s: MatchResult[GenTraversableOnce[T]], containMatcher: ContainOnlyMatcher[T]) extends AbstractContainMatchResult[T] { outer =>
  val matcher = containMatcher
  def inOrder = new ContainOnlyInOrderMatchResult(s, containMatcher.inOrder)
}
class ContainInOrderMatchResult[T]  private[specs2](val s: MatchResult[GenTraversableOnce[T]], containMatcher: ContainInOrderMatcher[T]) extends AbstractContainMatchResult[T] { outer =>
  val matcher = containMatcher
  def only = new ContainOnlyInOrderMatchResult(s, containMatcher.only)
}
class ContainOnlyInOrderMatchResult[T] private[specs2](val s: MatchResult[GenTraversableOnce[T]], containMatcher: Matcher[GenTraversableOnce[T]]) extends AbstractContainMatchResult[T] { outer =>
  val matcher = containMatcher
}
trait AbstractContainMatchResult[T] extends MatchResult[GenTraversableOnce[T]] {
  val matcher: Matcher[GenTraversableOnce[T]]
  protected val s: MatchResult[GenTraversableOnce[T]]
  val expectable = s.expectable
  lazy val matchResult = s(matcher)

  override def toResult = matchResult.toResult
  def not: MatchResult[GenTraversableOnce[T]] = matchResult.not
  def apply(matcher: Matcher[GenTraversableOnce[T]]): MatchResult[GenTraversableOnce[T]] = matchResult(matcher)
}

class ContainMatcher[T](expected: Seq[T], equality: (T, T) => Boolean = (_:T) == (_:T)) extends AbstractContainMatcher(expected, equality) {
  type M[T] = ContainMatcher[T]
  def create(seq: =>Seq[T], eq: (T, T) => Boolean) = new ContainMatcher[T](seq, eq)

  def apply[S <: GenTraversableOnce[T]](actual: Expectable[S]) = {
    val missing = expected.filterNot(e => actual.value.toList.exists(a => equality(a, e)))
    result(missing.isEmpty,
           actual.description + " contains " + qseq(expected),
           actual.description + " doesn't contain " + qseq(missing), actual)
  }
  def inOrder = new ContainInOrderMatcher(expected, equality)
  def only = new ContainOnlyMatcher(expected, equality)
  def exactlyOnce = new ContainExactlyOnceMatcher(expected, equality)
}

import data._
import scalaz.{std, syntax}
import std.stream._
import std.anyVal._
import intInstance._
import std.map._
import syntax.foldable._

class ContainExactlyOnceMatcher[T](expected: Seq[T], equality: (T, T) => Boolean = (_:T) == (_:T)) extends AbstractContainMatcher(expected, equality) {
  type M[T] = ContainExactlyOnceMatcher[T]
  def create(seq: =>Seq[T], eq: (T, T) => Boolean) = new ContainExactlyOnceMatcher[T](seq, eq)

  def apply[S <: GenTraversableOnce[T]](actual: Expectable[S]) = {
    val actualValues = actual.value.seq.toStream.map(e => (e, 1)).foldMap(Map(_))(mapMonoid[T, Int])

    result(expected.forall(e => actualValues.filter { case (k, v) => equality(k, e) }.values.sum == 1),
           actual.description + " contains exactly once " + qseq(expected),
           actual.description + " doesn't contain exactly once " + qseq(expected), actual)
  }
}

class ContainAnyOfMatcher[T](expected: Seq[T], equality: (T, T) => Boolean = (_:T) == (_:T)) extends AbstractContainMatcher(expected, equality) {
  type M[T] = ContainAnyOfMatcher[T]
  def create(seq: =>Seq[T], eq: (T, T) => Boolean) = new ContainAnyOfMatcher[T](seq, eq)

  def apply[S <: GenTraversableOnce[T]](actual: Expectable[S]) = {
    val contained = actual.value.toList.filter((a: T) => expected.exists((e: T) => equality(a, e)))
    result(contained.nonEmpty,
           actual.description + " contains " + qseq(contained),
           actual.description + " doesn't contain any of " + qseq(expected), actual)
  }
}

class ContainInOrderMatcher[T](expected: Seq[T], equality: (T, T) => Boolean = (_:T) == (_:T)) extends AbstractContainMatcher(expected, equality) {
  type M[T] = ContainInOrderMatcher[T]
  def create(seq: =>Seq[T], eq: (T, T) => Boolean) = new ContainInOrderMatcher[T](seq, eq)

  def apply[S <: GenTraversableOnce[T]](actual: Expectable[S]) = {
    result(inOrder(actual.value.toList, expected, equality),
           actual.description + " contains in order " + qseq(expected),
           actual.description + " doesn't contain in order " + qseq(expected), actual)
  }
  
  private def inOrder[T](l1: Seq[T], l2: Seq[T], equality: (T, T) => Boolean): Boolean = {
    (l1.toList, l2.toList) match {
      case (Nil, Nil)                 => true
      case (Nil, _)                   => false
      case (_, Nil)                   => true
      case (a1 :: rest1, a2 :: rest2) => equality(a1, a2) && inOrder(rest1, rest2, equality) || inOrder(rest1, l2, equality)
      case other => false
     }
  }

  def only: Matcher[GenTraversableOnce[T]] = (this and new ContainOnlyMatcher(expected, equality))
  override def not = new ContainInOrderMatcher(expected, equality) {
    override def apply[S <: GenTraversableOnce[T]](actual: Expectable[S]) = super.apply(actual).not
  }
}

class ContainOnlyMatcher[T](expected: Seq[T], equality: (T, T) => Boolean = (_:T) == (_:T)) extends AbstractContainMatcher(expected, equality) {
  type M[T] = ContainOnlyMatcher[T]
  def create(seq: =>Seq[T], eq: (T, T) => Boolean) = new ContainOnlyMatcher[T](seq, eq)

  def apply[S <: GenTraversableOnce[T]](traversable: Expectable[S]) = {
    val actual = traversable.value
    result(actual.toSeq.filter(a => expected.exists(e => equality(a, e))).size == expected.size && expected.size == actual.size,
           traversable.description + " contains only " + qseq(expected),
           traversable.description + " doesn't contain only " + qseq(expected), traversable)
  }
  def inOrder: Matcher[GenTraversableOnce[T]] = (this and new ContainInOrderMatcher(expected, equality))
  override def not = new ContainOnlyMatcher(expected, equality) {
    override def apply[S <: GenTraversableOnce[T]](actual: Expectable[S]) = super.apply(actual).not
  }
}

class ContainLikeMatcher[T](pattern: =>String, matchType: String) extends Matcher[GenTraversableOnce[T]] {
  def apply[S <: GenTraversableOnce[T]](traversable: Expectable[S]) = {
    val a = pattern
    result(traversable.value.exists(_.toString.matches(a)), 
           traversable.description + " contains "+matchType+ " " + q(a), 
           traversable.description + " doesn't contain "+matchType+ " " + q(a), traversable)
  }
  def onlyOnce = new ContainLikeOnlyOnceMatcher[T](pattern, matchType)
}

class ContainLikeOnlyOnceMatcher[T](pattern: =>String, matchType: String) extends Matcher[GenTraversableOnce[T]] {
  def apply[S <: GenTraversableOnce[T]](traversable: Expectable[S]) = {
    val a = pattern
    val matchNumber = traversable.value.toSeq.filter(_.toString.matches(a)).size
    val koMessage = 
      if (matchNumber == 0)
        traversable.description + " doesn't contain "+matchType+ " " + q(a)
      else
        traversable.description + " contains "+matchType+ " " + q(a) + " "+ (matchNumber qty "time")
        
    result(matchNumber == 1, 
           traversable.description + " contains "+matchType+ " " + q(a) + " only once", 
           koMessage, 
           traversable)
  }
}

/**
 * This matcher checks if a traversable has the same elements as another one (that is, recursively, in any order)
 */
class HaveTheSameElementsAs[T](l: =>Seq[T], equality: (T, T) => Boolean = (_:T) == (_:T)) extends AbstractContainMatcher(l, equality) {
  type M[T] = HaveTheSameElementsAs[T]
  def create(seq: =>Seq[T], eq: (T, T) => Boolean) = new HaveTheSameElementsAs[T](seq, eq)

  def apply[S <: GenTraversableOnce[T]](traversable: Expectable[S]) = {
    result(traversable.value.toSeq.sameElementsAs(l.toSeq, equality),
           traversable.value.toSeq.toDeepString + " has the same elements as " + q(l.toSeq.toDeepString),
           traversable.value.toSeq.toDeepString + " doesn't have the same elements as " + q(l.toSeq.toDeepString),
           traversable, l.toSeq.toDeepString, traversable.value.toSeq.toDeepString)
  }

}

abstract class AbstractContainMatcher[T](l: =>Seq[T], equality: (T, T) => Boolean = (_:T) == (_:T)) extends Matcher[GenTraversableOnce[T]] {
  type M[T] <: AbstractContainMatcher[T]
  def create(seq: =>Seq[T], eq: (T, T) => Boolean): M[T]

  /** use a specific equality function */
  def ^^[S](equality: (T, T) => Boolean) = create(l, equality)

  /** use a matcher function to define if 2 values are equal. The first value defines a matcher to use with the second one */
  def ^^[S](m: T => Matcher[T]) = create(l, (t1: T, t2: T) => m(t1).apply(Expectable(t2)).isSuccess)

  /** use a specific adaption function before checking for equality */
  def ^^^[S](adaptator: T => S) = create(l, (t1: T, t2: T) => adaptator(t1) == adaptator(t2))
}



class SizedMatcher[T : Sized](n: Int, sizeWord: String) extends Matcher[T] {
  def apply[S <: T](traversable: Expectable[S]) = {
    val s = implicitly[Sized[T]]
    val valueSize = s.size(traversable.value)
    result(valueSize == n,
           traversable.description + " have "+sizeWord+" "+ n,
           traversable.description + " doesn't have "+sizeWord+" " + n + " but "+sizeWord+" " + valueSize, traversable)
  }
}

class OrderingMatcher[T : Ordering] extends Matcher[Seq[T]] {
  def apply[S <: Seq[T]](traversable: Expectable[S]) = {
    result(traversable.value == traversable.value.sorted,
      traversable.description + " is sorted",
      traversable.description + " is not sorted", traversable)
  }
}


case class HaveOneElementLike[T, U](like: PartialFunction[T, MatchResult[U]]) extends Matcher[GenTraversableOnce[T]] {
  def apply[S <: GenTraversableOnce[T]](traversable: Expectable[S]) = {
    val results = traversable.value.toSeq.collect {
      case v if like.isDefinedAt(v) => (v, ResultExecution.execute(like(v).toResult))
    }
    val inTraversable = "in "+traversable.description+"\n"
    val (successes, failures) = results.partition(_._2.isSuccess)
    val failureMessages = failures.map { case (t, r) => t+": "+r }.mkString("\n", "\n", "")
    result(successes.nonEmpty,
           "some elements are not correct"+failureMessages,
           inTraversable+"no element is correct"+failureMessages,
           traversable)
  }
}

case class HaveAllElementsLike[T, U](like: PartialFunction[T, MatchResult[U]]) extends Matcher[GenTraversableOnce[T]] {
  def apply[S <: GenTraversableOnce[T]](traversable: Expectable[S]) = {
    val results = traversable.value.toSeq.collect {
      case v if like.isDefinedAt(v) => (v, ResultExecution.execute(like(v).toResult))
    }
    val inTraversable = "in "+traversable.description+"\n"
    val (successes, failures) = results.partition(_._2.isSuccess)
    val failureMessages = failures.map { case (t, r) => t+": "+r }.mkString("\n", "\n", "")
    result(failures.isEmpty,
           inTraversable+"all elements are correct",
           inTraversable+"some elements are not correct"+failureMessages,
           traversable)
  }
}

import control.NumberOfTimes._
import text.Plural._

trait ContainCheck[T] { outer =>
  def check: T => Result
  def checkNot: T => Result

  def messages(expectable: String, successes: Seq[Result], failures: Seq[Result]): (String, String)

  def not = new ContainCheck[T] {
    def check: T => Result = outer.checkNot
    def checkNot: T => Result = outer.check
    def messages(expectable: String, successes: Seq[Result], failures: Seq[Result]) = outer.messages(expectable, successes, failures)
  }
}

object ContainCheck {
  def genericMessages(expectable: String, successes: Seq[Result], failures: Seq[Result]) = {
    def elementsAre(results: Seq[Result]) =
      (if (results.size == (successes.size+failures.size)) "all elements are" else results.size.beQty("element"))+" ok"

    (elementsAre(successes)+successes.map(_.message).mkString("\n", "\n", "\n"),
     negateSentence(elementsAre(failures))+failures.map(_.message).mkString("\n", "\n", "\n"))
  }
}

case class ContainWithResult[T](check: ContainCheck[T], timesMin: Option[Times] = Some(1.times), timesMax: Option[Times] = None) {
  def apply[S <: GenTraversableOnce[T]](t: Expectable[S]) = {
    val seq = t.value.seq.toSeq
    val (successes, failures) = seq.map(check.check).partition(_.isSuccess)
    val (okMessage, koMessage) = check.messages(t.description, successes, failures)

    (timesMin, timesMax) match {
      case (None,             None)             => Matcher.result(successes.size == seq.size,                     okMessage, koMessage, t)
      case (Some(Times(min)), None)             => Matcher.result(successes.size >= min,                          okMessage, koMessage, t)
      case (None,             Some(Times(max))) => Matcher.result(successes.size <= max,                          okMessage, koMessage, t)
      case (Some(Times(min)), Some(Times(max))) => Matcher.result(successes.size >= min && successes.size <= max, okMessage, koMessage, t)
    }
  }

  def atLeastOnce           = atLeast(1.times)
  def atLeast(times: Times) = copy(timesMin = Option(times))

  def atMostOnce           = atMost(1.times)
  def atMost(times: Times) = copy(timesMax = Option(times))

  def between(min: Times, max: Times) = atLeast(min).atMost(max)
  def exactly(times: Times)           = atLeast(times).atMost(times)

  def forall = copy(timesMin = None, timesMax = None)

  def not = copy(check = check.not, timesMin = timesMax, timesMax = timesMin)
}

case class ContainWithMatcherSeq[T](checks: Seq[ContainCheck[T]],
                                    containsAtLeast: Boolean = true,
                                    containsAtMost: Boolean = false,
                                    checkOrder: Boolean = false) extends Matcher[GenTraversableOnce[T]] {

  def apply[S <: GenTraversableOnce[T]](t: Expectable[S]) = {
    val seq = t.value.seq.toSeq

    // results for each element, either checked in order or greedily from the list of checks
    val results: Seq[Result] =
      if (checkOrder) (seq zip checks).map { case (s, c) => c.check(s) }
      else            seq.foldLeft((Seq[Result](), checks)) { (res, cur) =>
        val (results, remainingChecks) = res
        val checksAgainstCurrentElement = remainingChecks.view.map(c => (c, c.check(cur)))
        (results :+ checksAgainstCurrentElement.map(_._2).foldLeft(Failure("there are no more available checks for "+cur): Result) { (res, cur) => cur or res }, checksAgainstCurrentElement.removeFirst(_._2.isSuccess).map(_._1))
      }._1

    lazy val (successes, failures) = results.partition(_.isSuccess)

    val qty =
      if      (containsAtLeast && containsAtMost) s"exactly ${checks.size}"
      else if (containsAtLeast)                   s"at least ${checks.size}"
      else if (containsAtMost)                    s"at most ${checks.size}"
      else                                        s"${checks.size}"

    val order = if (checkOrder) " in order" else ""
    val values = s"correct ${"value".plural(checks.size)}$order"
    val okMessage = s"${t.description} has $qty $values"
    val koMessage = negateSentence(okMessage) + (if (failures.isEmpty) "" else failures.mkString("\n", "\n", "\n"))

    (containsAtLeast, containsAtMost) match {
      case (true,  false) => Matcher.result(successes.size >= checks.size && checks.size <= seq.size, okMessage , koMessage, t)
      case (false, true)  => Matcher.result(successes.size <= checks.size && checks.size >= seq.size, okMessage , koMessage, t)
      case _              => Matcher.result(successes.size == seq.size,                               okMessage,  koMessage, t)
    }
  }

  def atLeast = copy(containsAtLeast = true, containsAtMost = false)
  def atMost  = copy(containsAtLeast = false, containsAtMost = true)
  def exactly = copy(containsAtLeast = true, containsAtMost = true)

  def inOrder = copy(checkOrder = true)

  override def not = copy(checks = checks.map(_.not), containsAtLeast = containsAtMost, containsAtMost = containsAtLeast)
}