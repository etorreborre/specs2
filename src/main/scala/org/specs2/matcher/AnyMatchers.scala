package org.specs2
package matcher
import execute._

trait AnyMatchers {
  class BeMatcher[T](other: =>T) extends Matcher[T] {
    def apply(t: =>T) = { 
	  val (a, b) = (t, other)
	  if (a == b) Success(q(a) + " is equal to " + q(b)) 
	  else Failure(q(a) + " is not equal to " + q(b))
    }
  }
  def q(a: Any) = "'"+a+"'"
}
object AnyMatchers extends AnyMatchers