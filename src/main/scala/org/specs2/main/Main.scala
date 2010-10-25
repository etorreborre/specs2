package org.specs2
package main

trait Main extends Arguments {
  def main(args: Array[String]): Unit
}
trait Arguments {
  protected def args: String = ""
  private[specs2] def arguments = new LineArgs(args)
}
case class LineArgs(arguments: String) {
  private def names = arguments.split(" ").map(_.replace("-", "")).toList
  def contains(name: String) = names.contains(name)
  override def toString = names.mkString
}