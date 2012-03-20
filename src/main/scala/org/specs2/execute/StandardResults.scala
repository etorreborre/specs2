package org.specs2
package execute

/**
 * This trait provides standard results which can be used in Fragments bodies
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
  // this will be removed in 2.10 and replaced by Predef.???
  def ??? : Nothing = throw new NotImplementedError("not implemented")
}
// this will be switched with the real error class in scala 2.10
case class NotImplementedError(msg: String) extends java.lang.Error(msg)

object StandardResults extends StandardResults