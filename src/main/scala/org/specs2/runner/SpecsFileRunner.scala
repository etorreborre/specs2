package org.specs2
package runner
import specification._
import reporter._

object SpecsFileRunner extends SpecificationsFinder with AConsoleReporter {
  def main(args: Array[String]): Unit = {
	val f = (e: Exception) => e.printStackTrace  
	val totalSpec = new Specification {
	  val Fragments = new Fragments(
	 		  specificationNames("src/test/scala", ".*Spec").
	            flatMap(s => createSpecification(s)).flatMap(_.Fragments.fragments :+ end))
	}
	reporter.report(totalSpec)
  }
}
