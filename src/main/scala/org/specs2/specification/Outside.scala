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

/**
 * The AroundOutside trait can be inherited by classes which will execute some code inside a given context, with a
 * function using that context and actions before and after if necessary.
 *
 * @see Example to understand why the type T must <% Result
 */
trait AroundOutside[T] { outer =>
  /** create a new context object */
  def outside: T
  /** something can be done before and after the whole execution */
  def around[R <% Result](a: =>R): Result

  def apply[R <% Result](a: T => R) = {
    around(Contexts.execute(outside)(a))
  }
}


