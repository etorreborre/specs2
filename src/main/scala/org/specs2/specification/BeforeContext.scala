package org.specs2
package specification

trait BeforeContext {
  def before: Any
  def apply[T](a: =>T) = { before; a }
}
