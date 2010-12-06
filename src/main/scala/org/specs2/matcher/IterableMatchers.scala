package org.specs2
package matcher

import control._
import text.Quote._
import collection.Iterablex._

/**
 * Matchers for iterables 
 */
trait IterableMatchers extends IterableBaseMatchers with IterableBeHaveMatchers
object IterableMatchers extends IterableMatchers

private[specs2]
trait IterableBaseMatchers extends LazyParameters { outer =>
  
  trait IterableMatcher[T] extends Matcher[Iterable[T]]
  
  /** 
   * match if an iterable contains (t).
   * This definition looks redundant with the one below but it is necessary to 
   * avoid the implicit argThat method from Mockito to infer an improper matcher
   * @see the HtmlPrinterSpec failing with a NPE if that method is missing 
   */
  def contain[T](t: =>T): ContainMatcher[T] = contain(lazyfy(t))
  /** match if iterable contains (t1, t2) */
  def contain[T](t: LazyParameter[T]*): ContainMatcher[T] = new ContainMatcher(t:_*)
  
  /** match if iterable contains (x matches p) */
  def containPattern[T](t: =>String): IterableMatcher[T] = containLike(t, "pattern")
  /** match if iterable contains (x matches .*+a+.*) */
  def containMatch[T](t: =>String): IterableMatcher[T] = containLike(".*"+t+".*", "match")

  /** match if iterable.isEmpty */
  def empty[T] = beEmpty[T]
  /** match if iterable.isEmpty */
  def beEmpty[T] = new IterableMatcher[T] {
    def apply[S <: Iterable[T]](v: =>Expectable[S]) = {
      val iterable = v
      result(iterable.value.isEmpty, 
             iterable.description + " is empty", 
             iterable.description + " is not empty", iterable)
    }
  }

  /** match if iterable has size n */
  def haveSize[T](n: Int) = new IterableMatcher[T] {
    def apply[S <: Iterable[T]](v: =>Expectable[S]) = {
      val iterable = v
      result(iterable.value.size == n, 
             iterable.description + " have size " + n, 
             iterable.description + " doesn't have size " + n, iterable)
    }
  }
  
  private def containLike[T](pattern: =>String, matchType: String) = new IterableMatcher[T] {
    def apply[S <: Iterable[T]](v: =>Expectable[S]) = {
      val (a, iterable) = (pattern, v)
      result(iterable.value.exists(_.toString.matches(a)), 
    		     iterable.description + " contains "+matchType+ " " + q(a), 
    		     iterable.description + " doesn't contain "+matchType+ " " + q(a), iterable)
    }
  }
}

private[specs2]
trait IterableBeHaveMatchers extends LazyParameters { outer: IterableMatchers =>
  implicit def iterable[T](s: MatchResult[Iterable[T]]) = new IterableBeHaveMatchers(s)
  class IterableBeHaveMatchers[T](s: MatchResult[Iterable[T]]) {
    def contain(ts: LazyParameter[T]*) = s.apply(outer.contain(ts:_*))
    def containMatch(t: =>String) = s.apply(outer.containMatch(t))
    def containPattern(t: =>String) = s.apply(outer.containPattern(t))
    def empty = s.apply(outer.beEmpty[T])
    def beEmpty = s.apply(outer.beEmpty[T])
    def size(n: Int) = s.apply(outer.haveSize(n))
  }
}

class ContainMatcher[T](t: LazyParameter[T]*) extends Matcher[Iterable[T]] {
  def apply[S <: Iterable[T]](it: =>Expectable[S]) = {
    val (expected, iterable) = (t.toList.map(_.value), it)
    result(iterable.value.toList.intersect(expected).sameElementsAs(expected), 
           iterable.description + " contains " + q(expected.mkString(", ")), 
           iterable.description + " doesn't contain " + q(expected.mkString(", ")), iterable)
  }
  def inOrder = new ContainInOrderMatcher(t:_*)
}
class ContainInOrderMatcher[T](t: LazyParameter[T]*) extends Matcher[Iterable[T]] {
  def apply[S <: Iterable[T]](v: =>Expectable[S]) = {
    val (expected, iterable) = (t.toList.map(_.value), v)
    result(inOrder(iterable.value.toList, expected), 
           iterable.description + " contains in order " + q(expected.mkString(", ")), 
           iterable.description + " doesn't contain in order " + q(expected.mkString(", ")), iterable)
  }
  
  private def inOrder[T](l1: List[T], l2: List[T]): Boolean = {
   l1 match {      case Nil => l2 == Nil      case other => l2.headOption == l1.headOption && inOrder(l1.drop(1), l2.drop(1)) || inOrder(l1.drop(1), l2)    }  }}      