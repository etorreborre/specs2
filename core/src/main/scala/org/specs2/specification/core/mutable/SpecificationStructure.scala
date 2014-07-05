package org.specs2
package specification
package core
package mutable

import specification.dsl.mutable.MutableFragmentBuilder

trait SpecificationStructure extends specification.core.SpecificationStructure
  with MutableFragmentBuilder {

  def is = specificationStructure(Env())
}

