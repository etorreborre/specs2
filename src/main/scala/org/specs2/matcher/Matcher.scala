package org.specs2
package matcher
import specification._
import execute._
import AnyMatchers._

trait Matcher[-T] { outer =>
  def apply(t: =>T)(description: =>String): Result with MatchResult
  protected def result(test: =>Boolean, okMessage: =>String, koMessage: =>String): Result with MatchResult = {
	if (test) new MatchSuccess(okMessage, koMessage) 
	else new MatchFailure(koMessage, okMessage)
  }
  def not = new Matcher[T] {
    def apply(a: => T)(d: =>String) = outer(a)(d).not
  }
}
