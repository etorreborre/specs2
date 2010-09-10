package org.specs2
package io

/**
 * Trait representing an output with standard print functions
 */
trait Output {
  /**
   * prints several objects according to a format string (see Console.printf)
   */
  def printf(format: String, args: Any*)
  /**
   * prints an object with a newline
   */
  def println(m: Any) = printf("%s\n", m)
  /**
   * prints an object with no newline
   */
  def print(m: Any) = this.printf("%s", m)
  /**
   * flushes the content if necessary
   */
  def flush() = {}
  /**
   * prints stacktraces
   */
  def printStackTrace(t: Throwable) = t.printStackTrace(new java.io.PrintWriter(System.err) {
    override def println(s: String) = Output.this.println(s)
  })
}
