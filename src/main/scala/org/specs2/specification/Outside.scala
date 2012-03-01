package org.specs2
package specification

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
    ResultExecution.execute(outside)(a)
  }
}

/**
 * The AroundOutside trait can be inherited by classes which will execute some code inside a given context, with a
 * function using that context and actions before and after if necessary.
 *
 * @see Example to understand why the type T must <% Result
 */
trait AroundOutside[T] extends Outside[T] { outer =>
  /** something can be done before and after the whole execution */
  def around[R <% Result](a: =>R): Result

  override def apply[R <% Result](a: T => R) = {
    around(ResultExecution.execute(outside)(a))
  }
}


