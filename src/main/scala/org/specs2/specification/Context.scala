package org.specs2
package specification
import execute._

trait BeforeAfter extends Before with After {
  override def apply[T <% Result](a: =>T): Result = {
	before
	try { a	 } 
	finally { after }
  }  	
}
trait BeforeAfterAround extends Before with After with Around {
  override def apply[T <% Result](a: =>T): Result = {
	before
	try { around(a)	 } 
	finally { after }
  }  	
}