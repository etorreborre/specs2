package org.specs2
package reporter
import specification._
import io._
import control.Exceptions._

/**
 * A Reporter will report the execution of a Specification following 3 steps:
 * 1. an extraction of the Fragments to report (like creating Descriptions for junit)
 *   * filtering out some elements if necessary
 * 2. an ordering of the Fragments to execute:
 *   * action steps must be executed in order
 *   * dependency between Fragments can be specified
 *   * other Fragments can be executed concurrently (unless specified otherwise)
 * 3. a reporting to:
 *   * the console (ConsoleRunner or sbt)
 *   * a listener object (junit or sbt)
 *   * a file (html, xml, junit-report)
 *
 */
trait Reporter extends Output with Selection with ExecutionStrategy with Exporting {
  def report(spec: BaseSpecification): this.type = 
	  report(Fragments(() => SpecStart(name(spec)) +: spec.content.fragments :+ SpecEnd(name(spec))))
	
  def report(fragments: Fragments): this.type = {
    (select andThen execute andThen export)(fragments)
    this
  }
  
  def name(spec: BaseSpecification) = ClassName.className(spec)
  val configuration = new Configuration
}

trait AReporter {
  val reporter: Reporter
}

