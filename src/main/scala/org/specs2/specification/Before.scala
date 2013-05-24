package org.specs2
package specification

import execute._

/**
 * generic trait for Before, After, Around
 */
trait Context extends Scope {
  def apply[T : AsResult](a: =>T): Result
}

object Context {
  def compose(c1: Context, c2: Context): Context = new Context {
    def apply[T : AsResult](a: =>T): Result = c1(c2(a))
  }
}

/**
 * The Before trait can be inherited by classes representing a context
 * where an action must be executing before the main executable action
 * 
 * @see Example to understand why the type T must : AsResult
 */
trait Before extends Context { outer =>

  /** override this method to provide the before behavior */
  def before: Any
  /** 
   * execute an action returning a Result
   * and finally the before action.
   * 
   * The action will be aborted if the before block fails:
   *
   * - with an exception
   * - with a non-Success result
   * - with a non-Success match result
   */
  override def apply[T : AsResult](a: =>T): Result =
    ResultExecution.execute(before)((any: Any) => AsResult(a))

  /** compose the actions of 2 Before traits */
  def compose(b: Before): Before = new Before {
    def before = { b.before; outer.before }
  }

  /** sequence the actions of 2 Before traits */
  def andThen(b: Before): Before = new Before {
    def before = { outer.before; b.before }
  }

}
