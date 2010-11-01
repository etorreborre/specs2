package org.specs2
package specification

import matcher._

trait BaseSpecification extends SpecificationStructure with FragmentsBuilder with Matchers {
  def include(s: BaseSpecification) = group(s.content.fragments)
}
trait SpecificationStructure {
  def is: Fragments
  lazy val content = is
} 
