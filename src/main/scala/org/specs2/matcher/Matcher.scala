package org.specs2
package matcher
import specification._
import execute._
import AnyMatchers._

/**
 * <p>The <code>Matcher</code> trait is the base trait for Matchers.
 * 
 * This trait can be extended to provide an appropriate <code>apply</code> method that 
 * will check an expectable value <code>a: Expectable[T]</code></p>.
 * 
 * Children of that class can use the optional description string to provide enhanced failure messages.
 * 
 * <i>Implementation notes</i>:<ul>
 * <li>the parameter to the apply method must be a by-name parameter. This allows some values to be evaluated
 * only when necessary. For example in <code>a must (m1(b) and m2(c))</code> m2(c) will not be evaluated if m1(b) is false</li>
 * <li>However in the implementation of the apply function, it must be taken care of not evaluating the parameter twice. 
 * Assigning it to a val is the solution to this issue.</li>
 * <li>2 messages are included in the result of the apply function, to allow the easy creation of the negation of matchers
 *  with the not method.</li>
 * </ul>
 * 
 */
trait Matcher[-T] { outer =>

  def apply[S <: T](t: =>Expectable[S]): MatchResult[S]
  
  protected def result[S <: T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: Expectable[S]): MatchResult[S] = {
	Matcher.result(test, okMessage, koMessage, value) 
  }

  def not = new Matcher[T] {
    def apply[U <: T](a: =>Expectable[U]) = outer(a).not
  }
  def or[S <: T](m: =>Matcher[S]) = new Matcher[S] {
    def apply[U <: S](a: =>Expectable[U]) = {
      val value = a
      outer(value).or(m(value))
    }
  }
  def orSkip: Matcher[T] = new Matcher[T] {
    def apply[U <: T](a: =>Expectable[U]) = {
      val value = a
      outer(value) match {
    	case MatchFailure(_, ko, _) => MatchSkip(ko, value)
    	case other => other
      }
    }
  }
}

object Matcher {
  def result[T](test: =>Boolean, okMessage: =>String, koMessage: =>String, value: =>Expectable[T]): MatchResult[T] = {
	if (test) new MatchSuccess(okMessage, koMessage, value) 
	else new MatchFailure(okMessage, koMessage, value)
  }
}
