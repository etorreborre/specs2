package org.specs2
package matcher
import specification._
import execute._
import AnyMatchers._

trait Matcher[-T] {
  def apply(t: =>T)(description: (Any => String) = q(_)): Result
  protected def result(test: =>Boolean, okMessage: =>String, koMessage: =>String) = {
	if (test) Success(okMessage) 
	else Failure(koMessage)
  }
}