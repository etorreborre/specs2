package org.specs2
package matcher
import specification._
import execute._
import AnyMatchers._

trait Matcher[-T] { outer =>
  def apply[S <: T](t: =>Expectable[S]): MatchResult[S]
  
  protected def result[S <: T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: Expectable[S]): MatchResult[S] = {
	Matcher.result(test, okMessage, koMessage, value) 
  }

  def not = new Matcher[T] {
    def apply[U <: T](a: =>Expectable[U]) = outer(a).not
  }
  def or[S <: T](m: =>Matcher[S]) = new Matcher[S] {
    def apply[U <: S](a: =>Expectable[U]) = outer(a).or(m(a))
  }
}
object Matcher {
  def result[T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: =>Expectable[T]): MatchResult[T] = {
	if (test) new MatchSuccess(okMessage, koMessage, value) 
	else new MatchFailure(okMessage, koMessage, value)
  }
}
