package org.specs2
package specification
import execute._
import control.Exceptions._

/**
 * The After trait can be inherited by classes representing a context 
 * where an action must be executing after the main executable action
 * 
 * @see Example to understand why the type T must <% Result
 */
trait After {
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
}

