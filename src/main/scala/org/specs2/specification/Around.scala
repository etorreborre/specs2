package org.specs2
package specification

trait Around {
  def around[T]: T => T
  def apply[T](a: =>T) = around(a)
}

