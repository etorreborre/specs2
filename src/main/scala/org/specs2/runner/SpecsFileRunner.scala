package org.specs2
package runner

import reflect._
import io.FromSource
import main.Arguments
import specification._
import StandardFragments._
import reporter._

/**
 * This object finds specifications in the source directory, instantiate them as one big
 * specification and report them using the ConsoleReporter
 * 
 * @see org.specs2.io.FromSource for the definition of the source directory
 * @see org.specs2.main.Arguments for the regular expression defining the specification names
 * to extract and other options
 */
object SpecsFileRunner extends SpecificationsFinder {
  lazy val reporter = new ConsoleReporter {}
  
  def main(arguments: Array[String]): Unit = {
	  implicit val args = Arguments(arguments:_*)
	    
	  lazy val allFragments = Fragments {
      SpecStart(specName) +: Par() +:
      specifications.flatMap(_.content.fragments) :+
      SpecEnd(specName)
    } 
    lazy val specName = "Specifications matching "+args.specName+" in "+FromSource.srcDir+"\n"
    
	  reporter.report(new Specification { def is = allFragments })
  }

  private def specifications(implicit args: Arguments) = {
    specificationNames(FromSource.srcDir, args.specName).flatMap(createSpecification(_))
  }
}