package org.specs2
package specification

import control.Exceptions._
import execute._

/**
 * The Before trait can be inherited by classes representing a context
 * where an action must be executing before the main executable action
 * 
 * @see Example to understand why the type T must <% Result
 */
trait Before { outer =>
  /** override this method to provide the before behavior */
  def before: Any
  /** 
   * execute an action returning a Result
   * and finally the before action.
   * 
   * The action will be aborted if the before block fails
   */
  def apply[T <% Result](a: =>T): Result = { 
	  trye(before)(Error(_)).left.getOrElse(a)
  }
  
  /** compose the actions of 2 Before traits */
  def compose(b: Before): Before = new Before {
    def before = { b.before; outer.before }
  }

  /** sequence the actions of 2 Before traits */
  def then(b: Before): Before = new Before {
    def before = { outer.before; b.before }
  }
}
