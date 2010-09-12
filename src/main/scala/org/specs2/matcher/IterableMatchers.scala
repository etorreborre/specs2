package org.specs2
package matcher
import AnyMatchers._

trait IterableMatchers {
  def contain[T](t: =>T) = new Matcher[Iterable[Any]](){
    def apply(v: => Iterable[Any])(d: Any => String) = {
      val (a, iterable) = (t, v)
      result(iterable.exists(_ == a), d(iterable) + " contains " + q(a), d(iterable) + " doesn't contain " + q(a))
    }
  }
  private def containLike(pattern: =>String, matchType: String) = new Matcher[Iterable[Any]](){
    def apply(v: => Iterable[Any])(d: Any => String) = {
      val (a, iterable) = (pattern, v)
      result(iterable.exists(_.toString.matches(a)), 
    		 d(iterable) + " contains "+matchType+ " " + q(a), d(iterable) + " doesn't contain "+matchType+ " " + q(a))
    }
  }
  def containPattern(t: =>String) = containLike(t, "pattern")
  def containMatch(t: =>String) = containLike(".*"+t+".*", "match")
}
object IterableMatchers extends IterableMatchers