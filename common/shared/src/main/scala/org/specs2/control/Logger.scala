package org.specs2.control

trait Logger {
  def logThrowable(t: Throwable, verbose: Boolean = false)
}
