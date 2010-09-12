package org.specs2
package matcher
import AnyMatchers._

trait IterableMatchers {
  def contain[T](a: T) = new Matcher[Iterable[Any]](){
    def apply(v: => Iterable[Any])(d: Any => String) = {
      val iterable = v
      result(iterable.exists(_ == a), d(iterable) + " contains " + q(a), d(iterable) + " doesn't contain " + q(a))
    }
  }
}
object IterableMatchers extends IterableMatchers