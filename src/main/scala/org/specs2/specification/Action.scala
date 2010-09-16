package org.specs2
package specification
import control.Exceptions._
import execute._

/**
 * An action create a Step fragment that will either return an
 * Error if there is an exception or a Success.
 * 
 * It is usually used to do some initialisation or cleanup before or after all
 * the examples.
 * 
 * Note that a Step fragment will not be reported in the output.
 * 
 * An action can be created once and reused later or used only once:
 * `val first = new Action; first(println("do it"))`
 * `Action(println("do it")) ^ "example1" ! e1`
 * 
 */
object Action {
  def apply(a: =>Any) = new Action().apply(a)
}
class Action {
  def apply(a: =>Any) = Step({() =>
	trye(a)(Error(_)).left.getOrElse(Success())  
  })
}
