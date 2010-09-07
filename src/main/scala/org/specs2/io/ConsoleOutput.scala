package org.specs2
package io

trait ConsoleOutput extends Output {
  override def println(s: String): Unit = Console.println(s)
}
