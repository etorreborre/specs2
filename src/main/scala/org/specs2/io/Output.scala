package org.specs2
package io

/**
 * Abstract trait representing an output with standard print functions
 */
trait Output {
  /**
   * prints an object with a newline
   */
  def println(m: Any) = printf("%s\n", m)

  /**
   * prints several objects according to a format string (see Console.printf)
   */
  def printf(format: String, args: Any*)

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
