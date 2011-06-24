package org.specs2
package specification

import control.Exceptions._
import execute._

/**
 * The After trait can be inherited by classes representing a context
 * where an action must be executing after the main executable action
 * 
 * @see Example to understand why the type T must <% Result
 */
trait After extends Context with DelayedInit { outer =>
  /**
   * this method allows the after code to be executed after any other code from the body of the Context
   */
  override def delayedInit(x: => Unit): Unit = {
    try { x } finally { after }
  }

  /** override this method to provide the after behavior */
  def after: Any
  /** 
   * execute an action returning a Result
   * and finally the after action 
   */
  def apply[T <% Result](a: =>T): Result = {
	  try { return a } 
	  finally { after	}
  } 
  
  /** compose the actions of 2 After traits */
  def compose(a: After): After = new After {
    def after = { a.after; outer.after }
  }

  /** sequence the actions of 2 After traits */
  def then(a: After): After = new After {
    def after = { outer.after; a.after }
  }

}

