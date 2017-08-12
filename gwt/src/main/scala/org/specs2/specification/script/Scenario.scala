package org.specs2
package specification
package script

/**
 * A sequence of GWT steps.
 */
trait Scenario extends Script {
  type S <: Scenario
  def start: Scenario
  def end: Scenario

  def stepsNumbers: Seq[Int]
  def withTitle(t: String): S
}
