package org.specs2
package reporter
import specification._
import io._

trait Reporter extends Output with Fold {
  def report(spec: BaseSpecification): T = 
	report(SpecStart(name(spec)) +: spec.examples.fragments :+ SpecEnd(name(spec)))
	
  def report(fragments: List[Fragment]): T = fold(fragments)
  
  def name(spec: BaseSpecification) = ClassName.className(spec)
  val configuration = new Configuration
}

trait AReporter {
  val reporter: Reporter
}

