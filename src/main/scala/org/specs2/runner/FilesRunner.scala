package org.specs2
package runner

import reflect._
import io.FromSource
import main._
import ArgProperties._
import specification._
import StandardFragments._
import reporter._

/**
 * This trait finds specifications in the source directory, instantiate them as one big
 * specification and report them using the ConsoleReporter or the HtmlReporter depending on the presence of the html flag
 * 
 * @see org.specs2.io.FromSource for the definition of the source directory
 * @see org.specs2.main.Arguments for the regular expression defining the specification names
 * to extract and other options
 */
trait FilesRunner extends SpecificationsFinder {

  def main(arguments: Array[String]) {
    implicit val args = createArguments(arguments)
    beforeExecution

    val specs = specifications
    reporters foreach  { r =>
        specs.foreach(execute(_, r))
        afterExecution(specs)
    }
    if (reporters.isEmpty)
      println("No file to run because the arguments don't contain 'console' or 'html'\n")
  }

  /** print a message before the execution */
  protected def beforeExecution(implicit args: Arguments) = println("\nExecuting specifications matching "+args.specName+" in "+FromSource.srcDir+"\n")
  /** report a specification */
  protected def execute(s: SpecificationStructure, r: Reporter)(implicit args: Arguments) = r.report(s)
  /** print a message after the execution based on the number of specifications */
  protected def afterExecution(specs: Seq[SpecificationStructure])(implicit args: Arguments) = {
    if (specs.size > 1)
      println("Finished the execution of "+specs.size+" specifications\n")
    else
      println("No specification found matching "+args.specName+" in "+FromSource.srcDir+"\n")
  }

  /** @return the Arguments object depending on the command-line options */
  protected def createArguments(arguments: Array[String]) = Arguments(arguments:_*) <| ArgumentsArgs.args(offset=2)

  /** @return a reporter depending on the provided arguments */
  protected def reporters(implicit arguments: Arguments): List[Reporter] =
      List((arguments.contains("html"), new HtmlReporter {}),
           (arguments.contains("console"), new ConsoleReporter {})).collect { case (true, r) => r }

  /** @return the specifications to execute */
  protected def specifications(implicit args: Arguments): Seq[SpecificationStructure] =
    specificationClassNames(args).flatMap(createSpecification(_))

  /** @return the specifications class names to execute */
  protected def specificationClassNames(implicit args: Arguments) = specificationNames(FromSource.srcDir, args.specName)

}
