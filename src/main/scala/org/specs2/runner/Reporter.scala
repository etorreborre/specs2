package org.specs2.runner
import org.specs2._
import org.specs2.specification._
import org.specs2.io._

trait Reporter extends Output with Folder {
  def report(spec: Specification): Unit = report(spec.examples)
  def report(examples: Examples): Unit = {
	examples.fragments.foldLeft(initial) { (res, cur) => 
	  if (folder.isDefinedAt((res, cur))) folder.apply((res, cur)) 
	  else res
    }
  } 
}
trait Folder {
  type T
  val initial: T
  val folder: PartialFunction[(T, Fragment), T]
}

trait AReporter {
  val reporter: Reporter
}
import scala.collection.mutable.ListBuffer

trait MockOutput extends Output {
  val messages: ListBuffer[String] = new ListBuffer
  override def println(m: String): Unit = messages += m
}