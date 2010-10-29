package org.specs2
package matcher
import execute._
import reflect.Classes._
import specification._

trait AnyMatchers extends BeHaveAnyMatchers {
  /** Matches if the expectable is true */
  def beTrue = new BeTrueMatcher
  /** Matches if the expectable is false */
  def beFalse = beTrue.not

  def be[T <: AnyRef](t: =>T) = new Matcher[T] {
    def apply[S <: T](v: =>Expectable[S]) = {
      val (a, b) = (v, t)
      result(a.value eq b, a.description + " is " + q(b), a.description + " is not " + q(b), a) 
    }
  }

  def be_==[T](t: =>T) = beEqualTo(t)
  def beEqualTo[T](t: =>T) = new BeEqualTo(t)
  def equalTo[T](t: =>T) = beEqualTo(t)

  /** negate a matcher */
  def not[T](m: Matcher[T]) = m.not
  
  private[specs2] def q(a: Any) = "'"+a+"'" 
  def isBoolean(a: Any) = a match {
  	case b: Boolean => true
  	case _ => false
  }

  /** @return an object.toString() without quotes (used in messages creation) */
  private[specs2] def unq(a: Any)  = if (null == a) "null" else a.toString

}
class BeTrueMatcher extends Matcher[Boolean] {
  def apply[S <: Boolean](v: =>Expectable[S]) = {
    result(v.value, v.description + " is true", v.description + " is false", v) 
  }
}
class BeEqualTo[T](t: =>T) extends Matcher[T] {
  import AnyMatchers._
  def apply[S <: T](v: =>Expectable[S]) = {
    val (a, b) = (t, v)
    val (db, qa) = (b.description, q(a)) match {
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
    result(a == b.value, db + " is equal to " + qa, db + " is not equal to " + qa, b)
  }
}
object AnyMatchers extends AnyMatchers

trait BeHaveAnyMatchers { outer: AnyMatchers =>
  implicit def anyMatcher[T](s: MatchResult[T]) = new AnyBeHaveMatchers(s)
  class AnyBeHaveMatchers[T](s: MatchResult[T]) {
    def equalTo(t: T) = s.apply(outer.be_==(t))
  }
}