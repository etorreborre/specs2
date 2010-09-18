package org.specs2
package matcher
import execute._
import reflect.Classes._
import specification._
trait AnyMatchers {
  
  def beTrue = new Matcher[Boolean] {
    def apply[S <: Boolean : Expectable](v: =>S) = {
      val b = v
      result(b, desc + " is true", desc + " is false") 
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
class BeEqualTo[T](t: =>T) extends Matcher[T] {
  import AnyMatchers._
  def apply[S <: T : Expectable](v: =>S) = {
    val (a, b) = (t, v)
    val (db, qa) = (desc, q(a)) match {
      case (x, y) if (a != b && q(a) == q(b)) => {
	    val aClass = getClassName(x)
	    val bClass = getClassName(y)
	    if (aClass != bClass)
          (y + ": " + bClass, x + ": " + aClass)
        else
          (y, x + ". Values have the same string representation but possibly different types like List[Int] and List[String]")
	  }
      case other @ _ => other 
	}
    result(a == b, db + " is equal to " + qa, db + " is not equal to " + qa)
  }
}

object AnyMatchers extends AnyMatchers