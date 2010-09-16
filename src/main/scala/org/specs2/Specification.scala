package org.specs2
import specification._
import runner._
import matcher._
import control.Exceptions._

trait Specification extends BaseSpecification with MustExpectations with Matchers 
    with AConsoleReporter {
	
  def include(s: Specification) =  group(examples.fragments)
  override def main(args: Array[String]): Unit = {
	run(args) {  case e =>
	  Console.println("\nAn error occurred. " +
	  		          "Please create an issue on the http://code.google.com/specs2 website with the stacktrace below. Thanks.")
	  	e.printStackTrace 
	}
  }
  protected[specs2] def run(args: Array[String])(f: Exception => Unit): Either[Unit, reporter.T] = {
	trye(reporter.report(this))(f)
  }
}
