package org.specs2
package runner

import io.FromSource
import main._
import ArgProperties._
import specification._
import reporter._

/**
 * This trait finds specifications in the source directory, instantiate them as one big
 * specification and report them using the ConsoleReporter or the HtmlReporter depending on the presence of the html flag
 * 
 * @see org.specs2.io.FromSource for the definition of the source directory
 * @see org.specs2.main.Arguments for the regular expression defining the specification names
 * to extract and other options
 */
trait FilesRunner extends SpecificationsFinder with SystemExit {

  def main(arguments: Array[String]) {
    exitSystem(run(arguments))
  }

  def run(arguments: Array[String]) = {
    implicit val args = createArguments(arguments)
    beforeExecution

    val specs = specifications(path     = args.commandLine.value("filesrunner.path").getOrElse("**/*.scala"),
                               pattern  = args.commandLine.value("filesrunner.pattern").getOrElse(".*Spec"),
                               basePath = args.commandLine.value("filesrunner.basepath").getOrElse(FromSource.srcTestDir),
                               verbose  = isVerbose)

    val executed = specs.toList.map(reporter.report)
    afterExecution(specs)
    executed
  }

  def reporter(implicit args: Arguments) = new DefaultReporter with AllExporting {}

  def isVerbose(implicit args: Arguments) = args.commandLine.contains("filesrunner.verbose")

  /** print a message before the execution */
  protected def beforeExecution(implicit args: Arguments) {
    if (isVerbose) {
      println("\nExecuting specifications matching " + args.specName + " in " + FromSource.srcTestDir)
      println("exporters are "+reporter(args).exporters(args).map(_.getClass.getName).mkString(","))
    }
  }

  /** print a message after the execution based on the number of specifications */
  protected def afterExecution(specs: Seq[SpecificationStructure])(implicit args: Arguments) {
    if (isVerbose) {
      if (specs.isEmpty) println("No specification found matching "+args.specName+" in "+FromSource.srcTestDir+"\n")
      else               println("Finished the execution of "+specs.size+" specifications\n")
    }
  }

  /** @return the Arguments object depending on the command-line options */
  protected def createArguments(arguments: Array[String]) = Arguments(arguments:_*) <| ArgumentsArgs.args.report(offset=2)

  /** @return the specifications to execute */
  protected def specifications(implicit args: Arguments): Seq[SpecificationStructure] =
    specificationClassNames(args).flatMap(name => createSpecification(name, verbose = args.commandLine.contains("verbose")))

  /** @return the specifications class names to execute */
  protected def specificationClassNames(implicit args: Arguments) = specificationNames(FromSource.srcTestDir, args.specName, verbose = args.commandLine.contains("verbose"))

}

object FilesRunner extends FilesRunner