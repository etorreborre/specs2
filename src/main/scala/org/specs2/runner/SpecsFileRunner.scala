package org.specs2
package runner
import specification._
import reporter._

object SpecsFileRunner extends SpecificationsFinder with AConsoleReporter {
  def main(arguments: Array[String]): Unit = {
	  val f = (e: Exception) => e.printStackTrace
	  val srcDir = "src/test/scala"
	  val pattern = ".*Spec"
	  val totalSpec = new Specification {
	    def is = new Fragments(() =>
	      SpecStart("Specifications matching "+pattern+" in "+srcDir+"\n") +: 
	   		specificationNames(srcDir, pattern).
	        flatMap { s => 
	          createSpecification(s)
	        }.flatMap(s => (SpecStart(ClassName.className(s)) :: s.content.fragments) :+ end), Args(arguments))
	  }
	  reporter.report(totalSpec)
  }
}
