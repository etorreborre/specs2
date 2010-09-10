package org.specs2.specification

trait AfterContext {
  def after: Any
  def apply[T](a: =>T) = {
	try { a	 } 
	finally { after }
  }  
}
