package org.specs2
package execute

/**
 * This trait provides standard results that can be used in
 * Fragments bodies
 */
trait StandardResults {
  def done = Success("DONE")
  def wontdo = Success("WONT DO")
  def todo = Pending("TODO")
  def pending = Pending("PENDING")
  def anError = Error("error")
  def success = Success("success")
  def failure = Failure("failure")
  def skipped = Skipped("skipped")
}
object StandardResults extends StandardResults