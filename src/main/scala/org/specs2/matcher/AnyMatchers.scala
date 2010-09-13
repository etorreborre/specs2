package org.specs2
package matcher
import execute._

trait AnyMatchers {
  class BeMatcher[T](other: =>T) extends Matcher[T] {
    def apply(t: =>T)(d: Any => String) = { 
	  val (a, b) = (t, other)
	  result(a == b, d(a) + " is equal to " + q(b),
	                 d(a) + " is not equal to " + q(b))
    }
  }
  
  def beTrue = new Matcher[Boolean] {
    def apply(v: =>Boolean)(d: Any => String) = { 
      val b = v
      result(b, d(" is true"), d(" is false")) 
    }
  }
  /**
   * Matches if b is false
   */
  def beFalse = beTrue.not
  
  private[specs2] def q(a: Any) = if (isBoolean(a)) "the value" else "'"+a+"'" 
  def isBoolean(a: Any) = a match {
  	case b: Boolean => true
  	case _ => false
  }

  /** @return an object.toString() without quotes (used in messages creation) */
  private[specs2] def unq(a: Any)  = if (null == a) "null" else a.toString

}
object AnyMatchers extends AnyMatchers {
}