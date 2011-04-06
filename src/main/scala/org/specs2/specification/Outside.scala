package org.specs2
package specification

import control.Exceptions._
import execute._

/**
 * The Outside trait can be inherited by classes which will
 * execute some code inside the outside method provided by the context.
 * 
 * This can be used for example to execute some code inside a webapp session, using the session object to
 * create expectations
 * 
 * @see Example to understand why the type T must <% Result
 */
trait Outside[T] { outer =>
  def outside: T
  def apply[R <% Result](a: T => R) = {
    Contexts.execute(outside)(a)
  }
}

