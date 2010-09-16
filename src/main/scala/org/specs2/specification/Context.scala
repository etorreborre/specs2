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
trait BeforeAfter extends Before with After {
  override def apply[T <% Result](a: =>T): Result = {
	super[After].apply(super[Before].apply(a))
  }  	
}

/**
 * The BeforeAfter trait allows to declare before, around and after actions
 * 
 * @see Before
 * @see After
 * @see Around
 */
trait BeforeAfterAround extends BeforeAfter with Around {
  override def apply[T <% Result](a: =>T): Result = {
	super[BeforeAfter].apply(super[Around].apply(a))
  }  	
}