package org.specs2
package specification

class ExamplesSpec extends Specification {
  val examples: Examples =
"""
  In a Specification, the spec variable stores an instance of the Examples class,
  which is merely a list of fragments. Those fragments are either:
  
   * Text elements
   * Example elements, with an executable block returning
   * SpecificationExamples elements which are examples coming from another specification
"""
}