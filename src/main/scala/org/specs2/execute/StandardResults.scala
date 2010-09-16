package org.specs2
package execute

/**
 * This trait provides standard results that can be used in
 * examples bodies
 */
trait StandardResults {
  def done = Success("DONE")
  def todo = Pending("TODO")
  def pending = Pending("PENDING")
  def error = Error("error")
  def success = Success("success")
  def failure = Failure("failure")
}