package org.specs2
package execute

/**
 * This trait provides standard results which can be used in Fragments bodies
 */
trait StandardResults {
  def done = Success("DONE")
  def wontdo = Success("WONT DO")
  def todo = Pending("TODO")
  def anError = Error("error")
  def success = Success("success")
  def failure = Failure("failure")

  def pending(message: String): Pending = Pending(message)
  def pending: Pending = pending("PENDING")
  def pending[R : AsResult](r: =>R): Pending = pending

  def skipped(message: String): Skipped = Skipped(message)
  def skipped: Skipped = skipped("skipped")
  def skipped[R : AsResult](r: =>R): Skipped = skipped
}

object StandardResults extends StandardResults