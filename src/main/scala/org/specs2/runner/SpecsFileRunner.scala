package org.specs2
package runner

object SpecsFileRunner extends SpecificationsFinder with AConsoleReporter {
  def main(args: Array[String]): Unit = {
	specificationNames("src/test/scala", ".*Spec") flatMap (createSpecification(_)) map (_.main(args))  
  }
}
