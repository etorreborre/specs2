package org.specs2
package specification
import control.Exceptions._
import execute._

trait Before {
  def before: Any
  def apply[T <% Result](a: =>T): Result = { 
	trye(before)(Error(_)).left.getOrElse(a)
  }
}
