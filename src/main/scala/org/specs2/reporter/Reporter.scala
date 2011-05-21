package org.specs2
package reporter

import org.specs2.internal.scalaz._
import Scalaz._
import control.Exceptions._
import main.Arguments
import io._
import reflect._
import specification._

/**
 * A Reporter will report the execution of a Specification following 3 steps:
 * 
 * 1. an extraction of the Fragments to report (like creating Descriptions for JUnit)
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
trait Reporter extends
       Selection
  with Sequence
  with ExecutionStrategy 
  with Exporting {

  /**
   * report Fragments by:
   *   * extracting arguments from the Fragments
   *   * selecting them
   *   * sequencing fragments in groups
   *   * executing fragments
   *   * exporting the results to the output format 
   *   
   * @return the reporter
   */
  def report(spec: SpecificationStructure)(implicit arguments: Arguments): this.type = {
    spec.content |> select |> sequence |> execute |> export(spec)
    this
  }
}
