package org.specs2
package control

trait ThrowablexContext {
  val cause = new IllegalArgumentException("cause")
  val e = new Exception("message", cause)
  val trace = e.getStackTrace()(0)
}
object ThrowablexContext extends ThrowablexContext