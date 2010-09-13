package org.specs2
package matcher
import AnyMatchers._

trait IterableMatchers {
  def contain[T](t: =>T) = new Matcher[Iterable[Any]](){
    def apply(v: => Iterable[Any])(d: =>String) = {
      val (a, iterable) = (t, v)
      result(iterable.exists(_ == a), d + " contains " + q(a), d + " doesn't contain " + q(a))
    }
  }
  private def containLike(pattern: =>String, matchType: String) = new Matcher[Iterable[Any]](){
    def apply(v: => Iterable[Any])(d: =>String) = {
      val (a, iterable) = (pattern, v)
      result(iterable.exists(_.toString.matches(a)), 
    		 d + " contains "+matchType+ " " + q(a), d + " doesn't contain "+matchType+ " " + q(a))
    }
  }
  def containPattern(t: =>String) = containLike(t, "pattern")
  def containMatch(t: =>String) = containLike(".*"+t+".*", "match")
}
object IterableMatchers extends IterableMatchers