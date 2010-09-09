package org.specs2
package runner
import specification._
import io._

trait Reporter extends Output with Folder {
  def report(spec: Specification): Unit = report(spec.examples)
  def report(examples: Examples): Unit = {
	examples.fragments.foldLeft(initial)(folder)
  } 
}
trait Folder {
  type T
  def initial: T
  val folder: Function2[T, Fragment, T]
}

trait AReporter {
  val reporter: Reporter
}
import scala.collection.mutable.ListBuffer

trait MockOutput extends Output {
  val messages: ListBuffer[String] = new ListBuffer
  override def println(m: String): Unit = messages += m
}