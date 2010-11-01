package org.specs2
package matcher

import scalaz._
import execute._
import AnyMatchers._

class Expectable[+T](t: =>T) { outer =>
  protected val desc: Option[String] = None
  lazy val value = t
  
  def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = {
	  m.apply(this) 
  }
  
  def description = d(value)
  /** @return the description of the matched value, quoted. */
  protected def d[T](value: =>T) = desc  match {
    case None => if (isBoolean(value)) "the value" else q(value)
    case Some(de: String) => de + (if (!value.toString.isEmpty && !isBoolean(value)) " " + q(value) else "")
  }
  /** @return the description of the matched value, unquoted. */
  protected def dUnquoted[T](value: T) = desc match {
    case None => unq(value)
    case Some(de) => de + " " + unq(value)  
  }
}

object Expectable {
  implicit def ExpectableFunctor[T](e: Expectable[T]): Functor[Expectable] = new Functor[Expectable] {
    def fmap[A, B](r: Expectable[A], f: A => B) = new Expectable(f(r.value)) {
      override protected val desc: Option[String] = r.desc
    }
  }
}