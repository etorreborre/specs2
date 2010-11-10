package org.specs2
package reporter

import control.Exceptions._
import main.Arguments
import io._
import reflect._
import specification._

/**
 * A Reporter will report the execution of a Specification following 3 steps:
 * 
 * 1. an extraction of the Fragments to report (like creating Descriptions for junit)
 *   * filtering out some elements if necessary
 *   
 * 2. an ordering of the Fragments to execute:
 *   * action steps must be executed in order
 *   * dependency between Fragments can be specified
 *   * other Fragments can be executed concurrently (unless specified otherwise)
 *   
 * 3. a reporting to:
 *   * the console (ConsoleRunner or sbt)
 *   * a listener object (junit or sbt)
 *   * a file (html, xml, junit-report)
 *
 */
private[specs2]
trait Reporter extends Selection 
  with ExecutionStrategy 
  with Exporting {
  /**
   * report the Fragments of a BaseSpecification
   * @return the reporter
   */
  def report(spec: BaseSpecification)(implicit arguments: Arguments): this.type = report(spec.content)
   	  
  /**
   * report Fragments by:
   *   * extracting arguments from the Fragments
   *   * selecting / ordering fragments
   *   * executing fragments
   *   * exporting the results to the output format 
   *   
   * @return the reporter
   */
  def report(fragments: Fragments)(implicit arguments: Arguments): this.type = {
    (select andThen execute andThen export)(fragments)
    this
  }
}
