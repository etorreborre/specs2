package org.specs2.specification

trait Expectations {
  implicit def theValue[T](t: =>T) = new Expectable(t)
}
class Expectable[T](t: =>T) {
  def must_==(other: =>T) = {
	val (a, b) = (t, other)
	if (a == b) Success(a + " is equal to " + b) 
	else Failure(a + " is not equal to " + b)
  }
}
trait Result { def message: String }
case class Success(message: String = "") extends Result
case class Failure(message: String = "") extends Result
case class Error(message: String = "") extends Result
case class Pending(message: String = "") extends Result
case class Skipped(message: String = "") extends Result
