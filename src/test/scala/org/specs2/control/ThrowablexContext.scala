package org.specs2
package control

trait ThrowablexContext {
  lazy val cause = new IllegalArgumentException("cause")
  lazy val e = new Exception("message", cause)
  lazy val trace = e.getStackTrace()(0)
}
object ThrowablexContext extends ThrowablexContext