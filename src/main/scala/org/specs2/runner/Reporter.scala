package org.specs2
package runner
import specification._
import io._

trait Reporter extends Output with Folder {
  def report(spec: Specification): Unit = report(SpecStart(name(spec)) +: spec.examples.fragments :+ SpecEnd(name(spec)))
  def report(fragments: List[Fragment]): Unit = {
	fragments.foldLeft(initial)(folder)
  } 
  def name(spec: Specification) = ClassName.className(spec)
}
trait Folder {
  type T
  def initial: T
  val folder: Function2[T, Fragment, T]
}

trait AReporter {
  val reporter: Reporter
}

