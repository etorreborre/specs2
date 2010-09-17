package org.specs2
package specification

class FragmentsSpec extends Specification {
  val Fragments: Fragments =
"""
  In a Specification, the spec variable stores an instance of the Fragments class,
  which is merely a list of fragments. Those fragments are either:
  
   * Text elements
   * Example elements, with an executable block returning
   * SpecificationFragments elements which are Fragments coming from another specification
"""
}