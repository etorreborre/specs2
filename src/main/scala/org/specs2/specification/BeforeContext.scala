package org.specs2.specification

trait BeforeContext {
  def before: Any
  def apply[T](a: =>T) = { before; a }
}
