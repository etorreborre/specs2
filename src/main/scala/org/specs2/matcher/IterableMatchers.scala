package org.specs2
package matcher
import AnyMatchers._
import specification._

trait IterableMatchers {
  trait IterableMatcher[T] extends Matcher[Iterable[T]]
  def contain[T](t: =>T): IterableMatcher[T] = new IterableMatcher[T] {
    def apply[S <: Iterable[T]](v: => Expectable[S]) = {
      val (a, iterable) = (t, v)
      result(iterable.value.exists(_ == a), 
    		 iterable.description + " contains " + q(a), 
    		 iterable.description + " doesn't contain " + q(a), iterable)
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