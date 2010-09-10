package org.specs2
package io

trait ConsoleOutput extends Output {
  override def printf(s: String, args: Any*): Unit = Console.printf(s, args:_*)
  override def flush() = Console.flush()
}
