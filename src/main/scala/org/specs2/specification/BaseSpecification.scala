package org.specs2
package specification
import matcher._

trait BaseSpecification extends SpecificationStructure with FragmentsBuilder with Matchers {
  def include(s: BaseSpecification) =  group(content.fragments)
}
trait SpecificationStructure {
  val content: Fragments
} 
