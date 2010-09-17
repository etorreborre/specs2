package org.specs2
package reporter
import specification._
import io._

trait Reporter extends Output with Folder {
  def report(spec: BaseSpecification): T = 
	report(SpecStart(name(spec)) +: spec.Fragments.fragments :+ SpecEnd(name(spec)))
	
  def report(fragments: List[Fragment]): T = {
	fragments.foldLeft(initial)(folder)
  } 
  
  def name(spec: BaseSpecification) = ClassName.className(spec)
  val configuration = new Configuration
}
trait Folder {
  type T
  def initial: T
  val folder: Function2[T, Fragment, T]
}

trait AReporter {
  val reporter: Reporter
}

