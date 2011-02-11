package org.specs2
package runner

import reflect._
import io.FromSource
import main._
import control.Properties._
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

  def main(arguments: Array[String]): Unit = {
    implicit val args = Arguments(arguments:_*) <| ArgumentsArgs.args(offset=2)
    println("\nExecuting specifications matching "+args.specName+" in "+FromSource.srcDir+"\n")

    val reporter =
      if (arguments.contains("html"))
        Some(new HtmlReporter {})
      else if (arguments.contains("console"))
        Some(new ConsoleReporter {})
      else None

    reporter match {
      case Some(r) => {
        val specs = specifications
        specs.foreach(r.report)
        println("Finished the execution of "+specs.size+" specifications\n")
      }
      case None => println("No file to run because the arguments don't contain 'console' or 'html'\n")
    }
  }

  private def specifications(implicit args: Arguments) =
    specificationNames(FromSource.srcDir, args.specName).flatMap(createSpecification(_))

}
object FilesRunner extends FilesRunner