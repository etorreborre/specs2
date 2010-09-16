package org.specs2
package runner
import specification._

object SpecsFileRunner extends SpecificationsFinder with AConsoleReporter {
  def main(args: Array[String]): Unit = {
	val f = (e: Exception) => e.printStackTrace  
	val totalSpec = new Specification {
	  val examples = new Examples(
	 		  specificationNames("src/test/scala", ".*Spec").
	            flatMap(s => createSpecification(s)).flatMap(_.examples.fragments :+ end))
	}
	reporter.report(totalSpec)
  }
}
