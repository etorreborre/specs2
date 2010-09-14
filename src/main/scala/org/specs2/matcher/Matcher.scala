package org.specs2
package matcher
import specification._
import execute._
import AnyMatchers._

trait Matcher[-T] { outer =>
  def apply[S <: T : Expectable](t: =>S): MatchResult[S]
  
  protected implicit def desc[S <: T : Expectable] = implicitly[Expectable[S]].description 
  protected def value[S <: T : Expectable] = implicitly[Expectable[S]]
  protected def result[S <: T : Expectable](test: =>Boolean, okMessage: =>String, koMessage: =>String): MatchResult[S] = {
	if (test) new MatchSuccess(okMessage, koMessage, value) 
	else new MatchFailure(okMessage, koMessage, value)
  }
  def not = new Matcher[T] {
    def apply[S <: T : Expectable](a: => S) = outer(a).not
  }
  def or[S <: T](m: =>Matcher[S]) = new Matcher[S] {
    def apply[U <: S : Expectable](a: =>U) = outer(a).or(m(a))
  }
}
