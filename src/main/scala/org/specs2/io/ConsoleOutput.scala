package org.specs2
package io

/**
 * Implementation of the Output trait using the standard Console output
 */
private[specs2]
trait ConsoleOutput extends Output {
  override def printf(s: String, args: Any*): Unit = Console.printf(s, args:_*)
  override def flush() = Console.flush()
}
