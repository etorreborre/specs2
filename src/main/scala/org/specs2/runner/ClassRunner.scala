package org.specs2
package runner

import reflect._
import io._
import main.Arguments
import control.Exceptions._
import specification._
import reporter._

/**
 * This class can be used to executed a Specification in the Console
 * by specifying its name as the first argument on the command line
 * 
 * @see specs2.run
 * @see org.specs2.main.Arguments for other command line options
 */
class ClassRunner extends Classes with ConsoleOutput {
	lazy val reporter: Reporter = new ConsoleReporter {}
	
  def main(arguments: Array[String]) = start(arguments:_*)
  
  def start(arguments: String*) = {
    if (arguments.length == 0)
      println("The first argument should at least be the specification class name")

    run(arguments.drop(1), createSpecification(arguments(0))) {  case e =>
	    println("\nAn error occurred. " +
              "Please create an issue on the http://specs2.org website with the stacktrace below. Thanks.")
	    e.printStackTrace 
	  }
  }
  
  protected[specs2] def run(args: Seq[String], specification: SpecificationStructure)(f: Exception => Unit) = {
	  trye(reporter.report(specification)(Arguments(args:_*).overrideWith(specification.content.arguments)))(f)
  }

  protected def createSpecification(className: String, classLoader: ClassLoader = Thread.currentThread.getContextClassLoader): SpecificationStructure = {
    tryToCreateObject[SpecificationStructure](className, loader = classLoader) match {
      case Some(s) => s
      case None => error("can not create specification: "+className)
    }
  }
}

