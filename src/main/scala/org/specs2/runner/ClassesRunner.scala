package org.specs2
package runner
import reflect._
import reporter._
import io._
import main.Main
import control.Exceptions._
import specification._

class ClassRunner extends Classes with ConsoleOutput with Main with AConsoleReporter {
	
  def main(arguments: Array[String]) = {
    if (arguments.length == 0)
      println("The first argument should be at least the specification class name")

    run(arguments.drop(1), createSpecification(arguments(0))) {  case e =>
	    println("\nAn error occurred. " +
              "Please create an issue on the http://code.google.com/specs2 website with the stacktrace below. Thanks.")
	    e.printStackTrace 
	  }
  }
  
  protected[specs2] def run(args: Array[String], specification: BaseSpecification)(f: Exception => Unit) = {
	  trye(reporter.report(specification))(f)
  }

  private def createSpecification(className: String): Specification = {
    tryToCreateObject[Specification](className, true, true) match {
      case Some(s) => s
      case None => error("can not create specification: "+className)
    }
  }
  private def fullClassName(packageName: String, className: String) = {
    if (packageName.trim.isEmpty) 
      className.trim
    else 
      packageName.trim+"."+className.trim
  }

}

