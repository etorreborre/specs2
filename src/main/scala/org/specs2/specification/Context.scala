package org.specs2
package specification

trait BeforeAfter extends Before with After {
  override def apply[T <: Result](a: =>T) = {
	before
	try { a	 } 
	finally { after }
  }  	
}
trait BeforeAfterAround extends Before with After with Around {
  override def apply[T <: Result](a: =>T): T = {
	before
	try { around(a)	 } 
	finally { after }
  }  	
}