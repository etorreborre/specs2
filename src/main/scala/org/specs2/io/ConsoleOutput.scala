package org.specs2.io

trait ConsoleOutput extends Output {
  override def println(s: String): Unit = Console.println(s)
}
