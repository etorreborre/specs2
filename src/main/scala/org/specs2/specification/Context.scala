package org.specs2
package specification

trait Context extends BeforeContext with AfterContext {
  override def apply[T](a: =>T) = {
	before
	try { a	 } 
	finally { after }
  }  	
}