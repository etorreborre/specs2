package org.specs2
package control

class ExceptionxContext {
  val cause = new IllegalArgumentException("cause")
  val e = new Exception("message", cause)
}
