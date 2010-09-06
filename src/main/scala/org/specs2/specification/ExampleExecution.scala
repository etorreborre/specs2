package org.specs2.specification

trait ExampleExecution {
  def execute(body: () => Result): Result = {
	try {
	  body() 
	} catch {
	  case e: Exception => Error(e.getMessage)
	}
  }
}