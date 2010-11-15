package org.specs2
package control

class ThrowablexContext {
  val cause = new IllegalArgumentException("cause")
  val e = new Exception("message", cause)
}
