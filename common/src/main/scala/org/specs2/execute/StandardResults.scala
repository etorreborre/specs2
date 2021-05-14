package org.specs2
package execute

/**
 * This trait provides standard results which can be used in Fragments bodies
 */
trait StandardResults:
  def done = Success("DONE")
  def wontdo = Success("WONT DO")
  def todo = pending("TODO")
  def anError: Error = anError("error")
  def anError(message: String): Error = Error(message)
  def success: Success = success("success")
  def success(message: String): Success = Success(message)

  def failure: Failure = failure("failure")
  def failure(message: String): Failure = Failure(message)

  def pending: Pending = pending("PENDING")
  def pending(message: String): Pending = Pending(message)
  def pending[R : AsResult](r: =>R): Pending = pending

  def skipped: Skipped = skipped("skipped")
  def skipped(message: String): Skipped = Skipped(message)
  def skipped[R : AsResult](r: =>R): Skipped = skipped

object StandardResults extends StandardResults
