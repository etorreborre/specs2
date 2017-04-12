package org.specs2
package io

/**
 * This trait represents an output with standard print functions
 */
trait Output {
  /**
   * print several objects according to a format string (see Console.printf)
   */
  def printf(format: String, args: Any*)
  /**
   * print an object with a newline
   */
  def println(m: Any) = printf("%s\n", m)
  /**
   * print an object with no newline
   */
  def print(m: Any) = this.printf("%s", m)
  /**
   * flush the content if necessary
   */
  def flush() = {}
  /**
   * print stacktraces
   */
  def printStackTrace(t: Throwable) = t.printStackTrace(new java.io.PrintWriter(System.err) {
    override def println(s: String) = Output.this.println(s)
  })
}
