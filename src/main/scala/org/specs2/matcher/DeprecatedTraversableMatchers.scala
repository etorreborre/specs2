package org.specs2
package matcher

import text.Quote._
import collection.Iterablex._
import execute._
import MatchersImplicits._
import scala.collection.GenTraversableOnce

/**
 * DEPRECATED
 */
private[specs2]
trait DeprecatedTraversableBaseMatchers { this: TraversableBaseMatchers =>
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
   * @deprecated(message="use containTheSameElementsAs(other)" instead, since="2.0")
   */
  def haveTheSameElementsAs[T](l: =>Traversable[T], equality: (T, T) => Boolean = (_:T) == (_:T)) =
    new HaveTheSameElementsAs(l.toSeq, equality)

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
    override def apply[S <: GenTraversableOnce[T]](actual: Expectable[S]) = super.apply(actual).negate
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
    override def apply[S <: GenTraversableOnce[T]](actual: Expectable[S]) = super.apply(actual).negate
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

