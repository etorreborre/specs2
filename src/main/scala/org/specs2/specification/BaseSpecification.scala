package org.specs2
package specification

import matcher._

/**
 * A Base specification contains the minimum elements for a Specification
 * 
 * * a Seq of Fragments, available through the SpecificationStructure trait
 * * methods for creating Fragments from the FragmentsBuilder trait
 * * methods for creating expectations with the Matchers trait
 *
 */
trait BaseSpecification extends SpecificationStructure with FragmentsBuilder with Matchers {
  def include(s: BaseSpecification) = group(s.content)
}

trait SpecificationStructure {
  def is: Fragments
  lazy val content = is
} 
