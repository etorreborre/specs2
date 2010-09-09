package org.specs2
package specification

trait Expectations {
  implicit def theValue[T](t: =>T) = new Expectable(t)
}
class Expectable[T](t: =>T) {
  def must_==(other: =>T) = {
	val (a, b) = (t, other)
	if (a == b) Success(q(a) + " is equal to " + q(b)) 
	else Failure(q(a) + " is not equal to " + q(b))
  }
  def q(a: Any) = "'"+a+"'"
}
sealed abstract class Result(val message: String = "", val expectationsNb: Int = 1)
case class Success(m: String = "")  extends Result(m)
case class Failure(m: String = "")  extends Result(m)
case class Error  (m: String = "")  extends Result(m)
case class Pending(m: String = "")  extends Result(m)
case class Skipped(m: String = "")  extends Result(m)
