package org.specs2
import specification._
import runner._
import matcher._

trait Specification extends BaseSpecification with MustExpectations with Matchers 
    with AConsoleReporter {
	
  def include(s: Specification) =  group(examples.fragments)
  override def main(args: Array[String]): Unit = {
	try {
	  reporter.report(this)
	} catch {
	  case e => {
	 	Console.println("\nAn error occurred. " +
	  		"Please create an issue on the http://code.google.com/specs2 website with the stacktrace below. Thanks.")
	  	e.printStackTrace 
	  }
	}
  }
}
