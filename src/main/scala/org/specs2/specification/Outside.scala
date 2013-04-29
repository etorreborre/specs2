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
 * @see Example to understand why the type T must : AsResult
 */
trait Outside[+T] { outer =>
  def outside: T
  def apply[R : AsResult](a: T => R) = AsResult(applyOutside(a))
  def applyOutside[R : AsResult](a: T => R) = a(outside)
}

/**
 * The AroundOutside trait can be inherited by classes which will execute some code inside a given context, with a
 * function using that context and actions before and after if necessary.
 *
 * @see Example to understand why the type T must : AsResult
 */
trait AroundOutside[+T] extends Around with Outside[T] { outer =>
  /** something can be done before and after the whole execution */
  def around[R : AsResult](a: =>R): Result

  override def apply[R : AsResult](a: T => R) = {
    val outsideParent: Outside[T] = this
    around(outsideParent.applyOutside(a))
  }
}


