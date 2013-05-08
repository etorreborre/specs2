package org.specs2
package matcher

import control._
import data.Sized
import text.Quote._
import text.Plural._
import collection.Iterablex._
import MatchersImplicits._
import scala.collection.{GenSeq, GenTraversableOnce, GenTraversable}
import execute.{ResultExecution, Result}

/**
 * Matchers for traversables
 */
trait TraversableMatchers extends TraversableBaseMatchers with TraversableBeHaveMatchers
object TraversableMatchers extends TraversableMatchers

private[specs2]
trait TraversableBaseMatchers extends LazyParameters with ImplicitParameters { outer =>
  
  trait TraversableMatcher[T] extends Matcher[GenTraversableOnce[T]]
  private type IP = ImplicitParam
  
  /** match if a traversable contains at least one element matching m */
  def contain[T](m: Matcher[T]): ContainWithMatcher[T] = new ContainWithMatcher(m)
  def contain[T](cm: ContainWithMatcherSeq[T]): ContainWithMatcherSeq[T] = cm
  /** match if a traversable contains at least one element matching t */
  def contain[T](t: =>T) = new ContainMatcher(Seq(t))

  def exactly[T](ms: Matcher[T]*)        : ContainWithMatcherSeq[T] = new ContainWithMatcherSeq(ms).exactly
  def exactly[T](ts: T*)(implicit p: IP) : ContainWithMatcherSeq[T] = new ContainWithMatcherSeq(ts.map(t => new BeTypedEqualTo(t))).exactly

  def allOf[T](ms: Matcher[T]*)        : ContainWithMatcherSeq[T] = new ContainWithMatcherSeq(ms).atLeast
  def allOf[T](ts: T*)(implicit p: IP) : ContainWithMatcherSeq[T] = new ContainWithMatcherSeq(ts.map(t => new BeTypedEqualTo(t))).atLeast

  def atLeast[T](ms: Matcher[T]*)        : ContainWithMatcherSeq[T] = new ContainWithMatcherSeq(ms).atLeast
  def atLeast[T](ts: T*)(implicit p: IP) : ContainWithMatcherSeq[T] = new ContainWithMatcherSeq(ts.map(t => new BeTypedEqualTo(t))).atLeast

  def atMost[T](ms: Matcher[T]*)        : ContainWithMatcherSeq[T] = new ContainWithMatcherSeq(ms).atMost
  def atMost[T](ts: T*)(implicit p: IP) : ContainWithMatcherSeq[T] = new ContainWithMatcherSeq(ts.map(t => new BeTypedEqualTo(t))).atMost

  implicit def containWithMatcherIsTraversableMatcher[T](cm: ContainWithMatcher[T]): Matcher[GenTraversableOnce[T]] = new Matcher[GenTraversableOnce[T]] {
    def apply[S <: GenTraversableOnce[T]](t: Expectable[S]) = cm(t)
  }

  implicit def containWithSingleValueIsContainWithMatcher[T](cv: ContainWithSingleValue[T]): ContainWithMatcher[T] = new ContainWithMatcher[T](new BeTypedEqualTo(cv.t()))

  implicit def containWithSingleValueIsMatcher[T](cv: ContainWithSingleValue[T]): Matcher[GenTraversableOnce[T]] =
    containWithSingleValueIsContainWithMatcher(cv)

  /** match if a traversable contains (seq2). n is a dummy paramter to allow overloading */
  def containAllOf[T](seq: Seq[T]): ContainMatcher[T] = new ContainMatcher(seq)
  /** match if traversable contains (t1, t2) */
  def contain[T](t: LazyParameter[T]*): ContainMatcher[T] = new ContainMatcher(t.map(_.value))
  /** match if a traversable contains one of (t1, t2) */
  def containAnyOf[T](seq: Seq[T]): ContainAnyOfMatcher[T] = new ContainAnyOfMatcher(seq)

  /** match if traversable contains (x matches p) */
  def containPattern[T](t: =>String): ContainLikeMatcher[T] = containLike[T](t, "pattern")
  /** match if traversable contains (x matches .*+t+.*) */
  def containMatch[T](t: =>String): ContainLikeMatcher[T] = containLike[T](".*"+t+".*", "match")

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
   * Matches if there is one element in the traversable verifying the `function` parameter: `(traversable.exists(function(_))`
   */
  def have[T](function: T => Boolean) = new Matcher[GenTraversableOnce[T]]{
    def apply[S <: GenTraversableOnce[T]](traversable: Expectable[S]) = {
      result(traversable.value.exists(function(_)),
             "at least one element verifies the property in " + traversable.description, 
             "no element verifies the property in " + traversable.description,
             traversable)
    }
  }

  /** Matches if there is at least one matching a "like" function */
  def haveOneElementLike[T, S](like: PartialFunction[T, MatchResult[S]]) = HaveOneElementLike(like)
  def oneElementLike[T, S](like: PartialFunction[T, MatchResult[S]]) = haveOneElementLike(like)

  /** Matches if all the elements are matching a "like" function */
  def haveAllElementsLike[T, S](like: PartialFunction[T, MatchResult[S]]) = HaveAllElementsLike(like)
  def allElementsLike[T, S](like: PartialFunction[T, MatchResult[S]]) = haveAllElementsLike(like)

  /**
   * Matches if there l contains the same elements as the Traversable `traversable`.<br>
   * This verification does not consider the order of the elements but checks the traversables recursively
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
    def contain(m: Matcher[T]) = s match {
      case NotMatch(r) => (new ContainWithMatcher[T](m.not))(r.expectable)
      case other       => (new ContainWithMatcher[T](m))(s.expectable)
    }
    def contain(cm: ContainWithMatcher[T]) = s match {
      case NotMatch(r) => (cm.not)(r.expectable)
      case other       => cm(s.expectable)
    }
    def contain(t: LazyParameter[T], ts: LazyParameter[T]*) = s match {
      case NotMatch(r) => new ContainNotMatchResult(s(outer.contain((t +: ts):_*)), outer.contain((t +: ts):_*))
      case other       => new ContainMatchResult(s, outer.contain((t +: ts):_*))
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
  def negate: MatchResult[GenTraversableOnce[T]] = matchResult.negate
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

case class ContainWithSingleValue[+T](t: () => T)

case class ContainWithMatcher[T](m: Matcher[T], timesMin: Option[Times] = Some(1.times), timesMax: Option[Times] = None) {
  def apply[S <: GenTraversableOnce[T]](t: Expectable[S]) = {
    val seq = t.value.seq.toSeq
    val (successes, failures) = seq.map(e => m(Expectable(e))).partition(_.isSuccess)
    val (okMessage, koMessage) = ((if (successes.size == seq.size) "all elements are" else successes.size.beQty("element"))+" ok"+successes.map(_.message).mkString("\n", "\n", "\n"),
                                  (if (failures.size == seq.size) "all elements are" else failures.size.beQty("element"))+" failing"+failures.map(_.message).mkString("\n", "\n", "\n"))

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

  def not = copy(m = m.not, timesMin = timesMax, timesMax = timesMin)
}

case class ContainWithMatcherSeq[T](ms: Seq[Matcher[T]], containsAtLeast: Option[Boolean] = None, containsAtMost: Option[Boolean] = None, checkOrder: Boolean = true) extends Matcher[GenTraversableOnce[T]] {
  def apply[S <: GenTraversableOnce[T]](t: Expectable[S]) = {
    val seq = t.value.seq.toSeq
    Matcher.result(true,                     "", "", t)
//    val (successes, failures) = seq.map(e => m(Expectable(e))).partition(_.isSuccess)
//    val (okMessage, koMessage) = ((if (successes.size == seq.size) "all elements are" else successes.size.beQty("element"))+" ok"+successes.map(_.message).mkString("\n", "\n", "\n"),
//      (if (failures.size == seq.size) "all elements are" else failures.size.beQty("element"))+" failing"+failures.map(_.message).mkString("\n", "\n", "\n"))
//
//    (timesMin, timesMax) match {
//      case (None,             None)             => Matcher.result(successes.size == seq.size,                     okMessage, koMessage, t)
//      case (Some(Times(min)), None)             => Matcher.result(successes.size >= min,                          okMessage, koMessage, t)
//      case (None,             Some(Times(max))) => Matcher.result(successes.size <= max,                          okMessage, koMessage, t)
//      case (Some(Times(min)), Some(Times(max))) => Matcher.result(successes.size >= min && successes.size <= max, okMessage, koMessage, t)
//    }
  }

  def atLeast = copy(containsAtLeast = Some(true))
  def atMost  = copy(containsAtMost = Some(true))
  def exactly = atLeast.atMost

  def inOrder = copy(checkOrder = true)

  override def not = copy(ms = ms.map(_.not), containsAtLeast = containsAtMost, containsAtMost = containsAtLeast)
}