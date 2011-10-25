package org.specs2
package runner

import reflect._
import io._
import main.Arguments
import control.Exceptions._
import specification._
import reporter._
import sys._
/**
 * This class can be used to executed a Specification in the Console
 * by specifying its name as the first argument on the command line
 * 
 * @see specs2.run
 * @see org.specs2.main.Arguments for other command line options
 */
class ClassRunner extends Classes with ConsoleOutput {
	lazy val reporter: Reporter = new ConsoleReporter {}

  protected val errorHandler: PartialFunction[Throwable, Unit] = {  case e =>
	  println("\nAn error occurred. " +
            "Please create an issue on the http://specs2.org website with the stacktrace below. Thanks.")
	  e.printStackTrace
  }

  def main(arguments: Array[String]) { start(arguments:_*) }
  
  def start(arguments: String*) = {
    if (arguments.length == 0)
      println("The first argument should at least be the specification class name")

    apply(createSpecification(arguments(0)))(Arguments(arguments.drop(1):_*))
  }

  /**
   * This method can be called directly from the console with the specs2.run object:
   *
   *     > specs2.run(spec1, spec2)
   * or  > import specs2._
   *     > run(spec1, spec2)
   *
   * If you want to pass specific arguments you can pass:
   *
   *    > import specs2.args._
   *    > specs2.run(spec1)(nocolor)
   *
   * Or you can set specific default with an implicit value:
   *
   *    > import specs2.args._
   *    > implicit val myargs = nocolor
   *    > specs2.run(spec1)
   */
  def apply(specifications: SpecificationStructure*)(implicit args: Arguments = Arguments()): Either[Reporter, Unit] = {
    specifications map { specification =>
      trye(reporter.report(specification)(args.overrideWith(specification.content.arguments)))(errorHandler)
    }
    Right(reporter)
  }

  protected def createSpecification(className: String, classLoader: ClassLoader = Thread.currentThread.getContextClassLoader): SpecificationStructure = {
    tryToCreateObject[SpecificationStructure](className, loader = classLoader) match {
      case Some(s) => s
      case None => error("can not create specification: "+className)
    }
  }
}

