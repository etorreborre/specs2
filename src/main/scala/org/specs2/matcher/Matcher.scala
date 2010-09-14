package org.specs2
package matcher
import specification._
import execute._
import AnyMatchers._

trait Matcher[-T] { outer =>
  def apply[S <: T : Expectable](t: =>S): Result with MatchResult
  protected implicit def desc[S <: T : Expectable] = implicitly[Expectable[S]].desc 
  protected def result(test: =>Boolean, okMessage: =>String, koMessage: =>String): Result with MatchResult = {
	if (test) new MatchSuccess(okMessage, koMessage) 
	else new MatchFailure(koMessage, okMessage)
  }
  def not = new Matcher[T] {
    def apply[S <: T : Expectable](a: => S) = outer(a).not
  }
  def or[S <: T](m: =>Matcher[S]) = new Matcher[S] {
    def apply[U <: S : Expectable](a: =>U) = outer(a).or(m(a))
  }
}
