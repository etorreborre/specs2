package org.specs2
package specification
import matcher._
import control._

trait BaseSpecification extends SpecificationStructure with FragmentsBuilder with Matchers with LazyParameters {
  def include(s: BaseSpecification) = group(s.content.fragments)
}
trait SpecificationStructure {
  def is: Fragments
  lazy val content = is
} 
