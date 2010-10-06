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
	  lazy val result = super[Before].apply(a)
	  super[After].apply(result)
  }  	
}

/**
 * The BeforeAfter trait allows to declare before, around and after actions
 * 
 * @see Before
 * @see After
 * @see Around
 */
trait BeforeAfterAround extends Before with After with Around {
  override def apply[T <% Result](a: =>T): Result = {
	  lazy val result = super[Around].apply(a)
	  lazy val resultWithBefore = super[Before].apply(result)
	  super[After].apply(resultWithBefore)
  }  	
}