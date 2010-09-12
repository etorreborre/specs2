package org.specs2
package specification
import control.Exceptions._
import execute._

class Action {
  def apply(a: =>Any) = Step({() =>
	tryo(a)(Error(_)).left.getOrElse(Success())  
  })
}
object Action {
  def apply(a: =>Any) = new Action().apply(a)
}