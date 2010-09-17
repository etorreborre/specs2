package org.specs2
package specification
import matcher._

trait BaseSpecification extends FragmentsBuilder  with Matchers {
  val examples: Fragments
  def include(s: BaseSpecification) =  group(examples.fragments)

} 
