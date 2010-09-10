package org.specs2
package io

trait ConsoleOutput extends Output {
  override def print(s: String): Unit = Console.print(s)
}
