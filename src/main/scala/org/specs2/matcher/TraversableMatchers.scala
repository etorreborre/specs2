package org.specs2
package matcher

import control._
import data.Sized
import text.Quote._
import text.Plural._
import collection.Iterablex._
import MatchersImplicits._

/**
 * Matchers for traversables 
 */
trait TraversableMatchers extends TraversableBaseMatchers with TraversableBeHaveMatchers
object TraversableMatchers extends TraversableMatchers

private[specs2]
trait TraversableBaseMatchers extends LazyParameters { outer =>
  
  trait TraversableMatcher[T] extends Matcher[Traversable[T]]
  
  /** 
   * match if an traversable contains (t).
   * This definition looks redundant with the one below but it is necessary to 
   * avoid the implicit argThat method from Mockito to infer an improper matcher
   * @see the HtmlPrinterSpec failing with a NPE if that method is missing 
   */
  def contain[T](t: =>T): ContainMatcher[T] = contain(lazyfy(t))
  /** match if traversable contains (t1, t2) */
  def contain[T](t: LazyParameter[T]*): ContainMatcher[T] = new ContainMatcher(t:_*)
  /** match if traversable contains one of (t1, t2) */
  def containAnyOf[T](t: LazyParameter[T]*): ContainAnyOfMatcher[T] = new ContainAnyOfMatcher(t:_*)

  /** match if traversable contains (x matches p) */
  def containPattern[T](t: =>String): ContainLikeMatcher[T] = containLike[T](t, "pattern")
  /** match if traversable contains (x matches .*+t+.*) */
  def containMatch[T](t: =>String): ContainLikeMatcher[T] = containLike[T](".*"+t+".*", "match")

  /**
   * Matches if there is one element in the traversable verifying the <code>function</code> parameter: <code>(traversable.exists(function(_))</code>
   */
  def have[T](function: T => Boolean) = new Matcher[Traversable[T]]{
    def apply[S <: Traversable[T]](traversable: Expectable[S]) = {
      result(traversable.value.exists(function(_)),
             "at least one element verifies the property in " + traversable.description, 
             "no element verifies the property in " + traversable.description,
             traversable)
    }
  }
  /**
   * Matches if there l contains the same elements as the Traversable <code>traversable</code>.<br>
   * This verification does not consider the order of the elements but checks the traversables recursively
   */
  def haveTheSameElementsAs[T](l: =>Traversable[T]) = new HaveTheSameElementsAs(l)

  private def containLike[T](pattern: =>String, matchType: String) =
    new ContainLikeMatcher[T](pattern, matchType) 

  /** match if there is a way to size T */
  def haveSize[T : Sized](n: Int) = new Matcher[T] {
    def apply[S <: T](traversable: Expectable[S]) = {
      val s = implicitly[Sized[T]]
      val valueSize = s.size(traversable.value)
      result(valueSize == n,
             traversable.description + " have size " + n,
             traversable.description + " doesn't have size " + n + " but size " + valueSize, traversable)
    }
  }

  /** any scala collection has a size */
  implicit def scalaTraversableIsSized[I <: TraversableOnce[_]] = new Sized[I] {
    def size(t: I) = t.size
  }
  /** any java collection has a size */
  implicit def javaCollectionIsSized[T <: java.util.Collection[_]]  = new Sized[T] {
    def size(t: T) = t.size()
  }
  /** a regular string has a size, without having to be converted to an Traversable */
  implicit def stringIsSized: Sized[String] = new Sized[String] {
    def size(t: String) = t.size
  }
}

private[specs2]
trait TraversableBeHaveMatchers extends LazyParameters { outer: TraversableMatchers =>
  implicit def traversable[T](s: MatchResult[Traversable[T]]) = new TraversableBeHaveMatchers(s)
  class TraversableBeHaveMatchers[T](s: MatchResult[Traversable[T]]) {
    def contain(t: LazyParameter[T], ts: LazyParameter[T]*) = new ContainMatchResult(s, outer.contain((t +: ts):_*))
    def containMatch(t: =>String) = s(outer.containMatch(t))
    def containPattern(t: =>String) = s(outer.containPattern(t))
    def have(f: T => Boolean) = s(outer.have(f))
  }
  implicit def sized[T : Sized](s: MatchResult[T]) = new HasSize(s)
  class HasSize[T : Sized](s: MatchResult[T]) {
    def size(n: Int) : MatchResult[T] = s(outer.haveSize[T](n))
  }
}
class ContainMatchResult[T](val s: MatchResult[Traversable[T]], containMatcher: ContainMatcher[T]) extends AbstractContainMatchResult[T] { outer =>
  val matcher = containMatcher
  def only = new ContainOnlyMatchResult(s, containMatcher.only)
  def inOrder = new ContainInOrderMatchResult(s, containMatcher.inOrder)
}
class ContainOnlyMatchResult[T](val s: MatchResult[Traversable[T]], containMatcher: ContainOnlyMatcher[T]) extends AbstractContainMatchResult[T] { outer =>
  val matcher = containMatcher
  def inOrder = new ContainOnlyInOrderMatchResult(s, containMatcher.inOrder)
}
class ContainInOrderMatchResult[T](val s: MatchResult[Traversable[T]], containMatcher: ContainInOrderMatcher[T]) extends AbstractContainMatchResult[T] { outer =>
  val matcher = containMatcher
  def only = new ContainOnlyInOrderMatchResult(s, containMatcher.only)
}
class ContainOnlyInOrderMatchResult[T](val s: MatchResult[Traversable[T]], containMatcher: Matcher[Traversable[T]]) extends AbstractContainMatchResult[T] { outer =>
  val matcher = containMatcher
}
trait AbstractContainMatchResult[T] extends MatchResult[Traversable[T]] {
  val matcher: Matcher[Traversable[T]]
  protected val s: MatchResult[Traversable[T]]
  val expectable = s.expectable
  lazy val matchResult = s(matcher)

  override def toResult = matchResult.toResult
  def not: MatchResult[Traversable[T]] = matchResult.not
  def apply(matcher: Matcher[Traversable[T]]): MatchResult[Traversable[T]] = matchResult(matcher)
}
class ContainMatcher[T](t: LazyParameter[T]*) extends Matcher[Traversable[T]] {
  def apply[S <: Traversable[T]](actual: Expectable[S]) = {
    val expected = t.toList.map(_.value)
    result(actual.value.toList.intersect(expected).sameElementsAs(expected),
           actual.description + " contains " + q(expected.mkString(", ")),
           actual.description + " doesn't contain " + q(expected.mkString(", ")), actual)
  }
  def inOrder = new ContainInOrderMatcher(t:_*)
  def only = new ContainOnlyMatcher(t:_*)
  def exactlyOnce = new ContainExactlyOnceMatcher(t:_*)
}

class ContainExactlyOnceMatcher[T](t: LazyParameter[T]*) extends Matcher[Traversable[T]] {
  def apply[S <: Traversable[T]](actual: Expectable[S]) = {
    val expected = t.toList.map(_.value)
    result(expected.forall(e => actual.value.toList.filter(_ == e).size == 1),
           actual.description + " contains exactly once " + q(expected.mkString(", ")),
           actual.description + " doesn't contain exactly once " + q(expected.mkString(", ")), actual)
  }
}

class ContainAnyOfMatcher[T](t: LazyParameter[T]*) extends Matcher[Traversable[T]] {
  def apply[S <: Traversable[T]](actual: Expectable[S]) = {
    val expected = t.toList.map(_.value)
    result(actual.value.toList.intersect(expected).nonEmpty,
           actual.description + " contains at least one of " + q(expected.mkString(", ")),
           actual.description + " doesn't contain any of " + q(expected.mkString(", ")), actual)
  }
}

class ContainInOrderMatcher[T](t: LazyParameter[T]*) extends Matcher[Traversable[T]] {
  def apply[S <: Traversable[T]](actual: Expectable[S]) = {
    val expected = t.toList.map(_.value)
    result(inOrder(actual.value.toList, expected),
           actual.description + " contains in order " + q(expected.mkString(", ")),
           actual.description + " doesn't contain in order " + q(expected.mkString(", ")), actual)
  }
  
  private def inOrder[T](l1: List[T], l2: List[T]): Boolean = {
   l1 match {
      case Nil => l2 == Nil
      case other => l2.headOption == l1.headOption && inOrder(l1.drop(1), l2.drop(1)) || inOrder(l1.drop(1), l2)
    }
  }

  def only: Matcher[Traversable[T]] = (this and new ContainOnlyMatcher(t:_*))
}

class ContainOnlyMatcher[T](t: LazyParameter[T]*) extends Matcher[Traversable[T]] {
  def apply[S <: Traversable[T]](traversable: Expectable[S]) = {
    val expected = t.toList.map(_.value)
    val actual = traversable.value
    result(actual.toList.intersect(expected).sameElementsAs(expected) && expected.size == actual.size,
           traversable.description + " contains only " + q(expected.mkString(", ")),
           traversable.description + " doesn't contain only " + q(expected.mkString(", ")), traversable)
  }
  def inOrder: Matcher[Traversable[T]] = (this and new ContainInOrderMatcher(t:_*))
}

class ContainLikeMatcher[T](pattern: =>String, matchType: String) extends Matcher[Traversable[T]] {
  def apply[S <: Traversable[T]](traversable: Expectable[S]) = {
    val a = pattern
    result(traversable.value.exists(_.toString.matches(a)), 
           traversable.description + " contains "+matchType+ " " + q(a), 
           traversable.description + " doesn't contain "+matchType+ " " + q(a), traversable)
  }
  def onlyOnce = new ContainLikeOnlyOnceMatcher[T](pattern, matchType)
}

class ContainLikeOnlyOnceMatcher[T](pattern: =>String, matchType: String) extends Matcher[Traversable[T]] {
  def apply[S <: Traversable[T]](traversable: Expectable[S]) = {
    val a = pattern
    val matchNumber = traversable.value.filter(_.toString.matches(a)).size
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
class HaveTheSameElementsAs[T] (l: =>Traversable[T]) extends Matcher[Traversable[T]] {
  def apply[S <: Traversable[T]](traversable: Expectable[S]) = {
    result(l.toSeq.sameElementsAs(traversable.value.toSeq),
           traversable.value.toSeq.toDeepString + " has the same elements as " + q(l.toSeq.toDeepString),
           traversable.value.toSeq.toDeepString + " doesn't have the same elements as " + q(l.toSeq.toDeepString),
           traversable)
  }
}

  