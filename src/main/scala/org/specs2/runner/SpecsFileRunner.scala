package org.specs2
package runner

import reflect._
import io.FromSource
import main.Arguments
import specification._
import reporter._

object SpecsFileRunner extends SpecificationsFinder with AConsoleReporter {
  def main(arguments: Array[String]): Unit = {
	  implicit val args = Arguments(arguments:_*)
	    
	  lazy val allFragments = Fragments {
      SpecStart(specName) +: 
      specifications.flatMap(include(_)) :+
      SpecEnd(specName)
    } 
    lazy val specName = "Specifications matching "+args.specName+" in "+FromSource.srcDir+"\n"
    
	  reporter.report(new Specification { def is = allFragments })
  }

  private def specifications(implicit args: Arguments) = {
    specificationNames(FromSource.srcDir, args.specName).flatMap(createSpecification(_))
  }
  private def include(s: BaseSpecification) = {
    (SpecStart(ClassName.className(s)) +: s.content.fragments) :+ SpecEnd(ClassName.className(s))
  }
}