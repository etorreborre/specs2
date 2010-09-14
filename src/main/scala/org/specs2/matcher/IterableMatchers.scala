package org.specs2
package matcher
import AnyMatchers._
import specification._

trait IterableMatchers {
  trait IterableMatcher[T] extends Matcher[Iterable[T]]
  def contain[T](t: =>T): IterableMatcher[T] = new IterableMatcher[T] {
    def apply[S <: Iterable[T] : Expectable](v: =>S) = {
      val (a, iterable) = (t, v)
      result(iterable.exists(_ == a), desc + " contains " + q(a), desc + " doesn't contain " + q(a))
    }
  }
  private def containLike(pattern: =>String, matchType: String) = new IterableMatcher[Any] {
    def apply[S <: Iterable[Any] : Expectable](v: =>S) = {
      val (a, iterable) = (pattern, v)
      result(iterable.exists(_.toString.matches(a)), 
    		 desc + " contains "+matchType+ " " + q(a), desc + " doesn't contain "+matchType+ " " + q(a))
    }
  }
  def containPattern(t: =>String) = containLike(t, "pattern")
  def containMatch(t: =>String) = containLike(".*"+t+".*", "match")
}
object IterableMatchers extends IterableMatchers