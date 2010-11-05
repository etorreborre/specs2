package org.specs2
package reporter

import control.Exceptions._
import io._
import reflect._
import specification._

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
private[specs2]
trait Reporter extends Output with Selection with ExecutionStrategy with Exporting {
  def report(spec: BaseSpecification): this.type = {
	  report(spec.content)
  }
   	  
  def report(fragments: Fragments): this.type = {
    implicit val args = fragments.arguments 
    (select andThen execute andThen export)(fragments)
    this
  }
}

private[specs2]
trait AReporter {
  val reporter: Reporter
}

