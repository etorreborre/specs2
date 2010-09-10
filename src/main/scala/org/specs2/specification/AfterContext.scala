package org.specs2
package specification

trait AfterContext {
  def after: Any
  def apply[T](a: =>T) = {
	try { a	 } 
	finally { after }
  }  
}

