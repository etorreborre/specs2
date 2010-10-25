package org.specs2
package matcher
import AnyMatchers._
import specification._
import control._

trait IterableMatchers extends LazyParameters {
  trait IterableMatcher[T] extends Matcher[Iterable[T]]
  
  def contain[T](t: LazyParameter[T]*): IterableMatcher[T] = new IterableMatcher[T] {
    def apply[S <: Iterable[T]](it: =>Expectable[S]) = {
      val (expected, iterable) = (t.toList.map(_.value), it)
      result(iterable.value.toList.intersect(expected) == expected, 
    		     iterable.description + " contains " + q(expected.mkString(", ")), 
    		     iterable.description + " doesn't contain " + q(expected.mkString(", ")), iterable)
    }
  }
  
  def containInOrder[T](t: LazyParameter[T]*): IterableMatcher[T] = new IterableMatcher[T] {
    def apply[S <: Iterable[T]](v: =>Expectable[S]) = {
      val (expected, iterable) = (t.toList.map(_.value), v)
      result(inOrder(iterable.value.toList, expected), 
             iterable.description + " contains in order " + q(expected.mkString(", ")), 
             iterable.description + " doesn't contain in order " + q(expected.mkString(", ")), iterable)
    }
  }
  private def inOrder[T](l1: List[T], l2: List[T]): Boolean = {
    l1 match {
      case Nil => l2 == Nil
      case other => l2.headOption == l1.headOption && inOrder(l1.drop(1), l2.drop(1)) || inOrder(l1.drop(1), l2)
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
  def containPattern[T](t: =>String) = containLike(t, "pattern")
  def containMatch[T](t: =>String): IterableMatcher[T] = containLike(".*"+t+".*", "match")
}
object IterableMatchers extends IterableMatchers