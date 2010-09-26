package org.specs2
package specification
import matcher._

trait BaseSpecification extends SpecificationStructure with FragmentsBuilder  with Matchers {
  def include(s: BaseSpecification) =  group(examples.fragments)
}
trait SpecificationStructure {
  val examples: Fragments
} 
