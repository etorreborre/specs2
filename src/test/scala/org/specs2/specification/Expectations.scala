package org.specs2.specification

trait Expectations {
  implicit def theValue[T](t: =>T) = new Expectable(t)
}
  class Expectable[T](t: =>T) {
	def must_==(other: T) = new Result("ok")
  }
  class Result(message: String)