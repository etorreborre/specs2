package org.specs2
package specification
import control.Exceptions._

class Action {
  def apply(a: =>Any) = Step({() =>
	tryo(a)(Error(_)).left.getOrElse(Success())  
  })
}