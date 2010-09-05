package org.specs2
import org.specs2.specification._

trait Specification extends ExamplesBuilder {
  val spec: Examples
  private implicit def asFragments: List[Fragment] = spec.fragments 
  def include(s: Specification) = asFragments
}