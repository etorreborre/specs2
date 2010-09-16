package org.specs2
package specification
import execute._

/**
 * The Around trait can be inherited by classes which will 
 * execute some code inside the around method provided by the context.
 * 
 * This can be used for example to execute some code inside a webapp session
 * 
 * @see Example to understand why the type T must <% Result
 */
trait Around {
  def around[T <% Result](t: =>T): Result
  def apply[T <% Result](a: =>T) = around(a)
}

