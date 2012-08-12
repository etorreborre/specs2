package org.specs2
package specification

import execute._

/**
 * The After trait can be inherited by classes representing a context
 * where an action must be executing after the main executable action
 * 
 * @see Example to understand why the type T must <% Result
 */
trait After extends Context { outer =>

  /** override this method to provide the after behavior */
  def after: Any
  /** 
   * execute an action returning a Result
   * and finally the after action 
   */
  def apply[T <% Result](a: =>T): Result = {
	  try { a }
	  finally { after	}
  } 
  
  /** compose the actions of 2 After traits */
  def compose(a: After): After = new After {
    def after = { a.after; outer.after }
  }

  /** sequence the actions of 2 After traits */
  @deprecated("then might become a keyword in future Scala versions. Use andThen instead", since = "1.13")
  def then(a: After): After = new After {
    def after = { outer.after; a.after }
  }
  /** sequence the actions of 2 After traits */
  def andThen(a: After): After = new After {
    def after = { outer.after; a.after }
  }

}

