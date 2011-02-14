package org.specs2
package control

object ThrowablexContext {
  val cause = new IllegalArgumentException("cause")
  val e = new Exception("message", cause)
}
