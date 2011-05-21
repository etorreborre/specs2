package org.specs2
package specification

import execute._

/**
 * The BeforeAfter trait allows to declare before and after actions
 * to execute before and after an Example 
 * 
 * @see Before
 * @see After
 */
trait BeforeAfter extends Before with After { outer =>
  override def apply[T <% Result](a: =>T): Result = {
	  lazy val result = super[Before].apply(a)
	  super[After].apply(result)
  }
  
  /** compose the actions of 2 BeforeAfter traits */
  def compose(b: BeforeAfter): BeforeAfter = new BeforeAfter {
    def before = { b.before; outer.before }
    def after = { b.after; outer.after}
  }

  /** sequence the actions of 2 BeforeAfter traits */
  def then(b: BeforeAfter): BeforeAfter = new BeforeAfter {
    def before = { outer.before; b.before }
    def after = { outer.after; b.after}
  }

}

/**
 * The BeforeAfter trait allows to declare before, around and after actions
 * 
 * @see Before
 * @see After
 * @see Around
 */
trait BeforeAfterAround extends Before with After with Around { outer =>
  override def apply[T <% Result](a: =>T): Result = {
	  lazy val result = super[Around].apply(a)
	  lazy val resultWithBefore = super[Before].apply(result)
	  super[After].apply(resultWithBefore)
  }
    /** compose the actions of 2 BeforeAfterAround traits */
  def compose(a: BeforeAfterAround): BeforeAfterAround = new BeforeAfterAround {
    def before = { a.before; outer.before }
    def after = { a.after; outer.after }
    def around[T <% Result](t: =>T): Result = {
      a.around(outer.around(t))
    }
  }

  /** sequence the actions of 2 BeforeAfterAround traits */
  def then(a: BeforeAfterAround): BeforeAfterAround = new BeforeAfterAround {
    def before = { outer.before; a.before }
    def after = { outer.after; a.after }
    def around[T <% Result](t: =>T): Result = {
      outer.around(a.around(t))
    }
  }
}
