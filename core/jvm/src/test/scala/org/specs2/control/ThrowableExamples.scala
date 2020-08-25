package org.specs2
package control

// This object is put in a separate file since there are some
// expectations related to the exact line numbers of thrown exceptions
object ThrowableExamples:
  lazy val cause = new IllegalArgumentException("cause")
  lazy val e = new Exception("message", cause)
  lazy val trace = e.getStackTrace()(0)
