package org.specs2
package specification
import execute._

trait Around {
  def around[T <% Result](t: =>T): Result
  def apply[T <% Result](a: =>T) = around(a)
}

