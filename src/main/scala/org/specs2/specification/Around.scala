package org.specs2
package specification

import execute._

/**
 * The Around trait can be inherited by classes which will
 * execute some code inside the around method provided by the context.
 * 
 * This can be used for example to execute some code inside a webapp session
 * 
 * @see Example to understand why the type T must <% Result
 */
trait Around extends Context with DelayedInit { outer =>
  /**
   * this method allows the around code to be executed around any other code from the body of the Context
   */
  override def delayedInit(x: => Unit): Unit = {
    around { x; Success() }
  }

  def around[T <% Result](t: =>T): Result
  def apply[T <% Result](a: =>T) = around(a)
  
  /** compose the actions of 2 Around traits */
  def compose(a: Around): Around = new Around {
    def around[T <% Result](t: =>T): Result = {
      a.around(outer.around(t))
    }
  }

  /** sequence the actions of 2 Around traits */
  def then(a: Around): Around = new Around {
    def around[T <% Result](t: =>T): Result = {
      outer.around(a.around(t))
    }
  }
}

