package org.specs2
package matcher
import execute._

trait AnyMatchers {
  class BeMatcher[T](other: =>T) extends Matcher[T] {
    def apply(t: =>T)(d: Any => String) = { 
	  val (a, b) = (t, other)
	  result(a == b,d(a) + " is equal to " + q(b),
	                d(a) + " is not equal to " + q(b))
    }
  }
  private[specs2] def q(a: Any) = "'"+a+"'"
  /** @return an object.toString() without quotes (used in messages creation) */
  private[specs2] def unq(a: Any)  = if (null == a) "null" else a.toString

}
object AnyMatchers extends AnyMatchers {
}