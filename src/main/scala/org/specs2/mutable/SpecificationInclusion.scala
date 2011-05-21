package org.specs2
package mutable

import main.Arguments
import specification.{Fragments, SpecificationStructure}

trait SpecificationInclusion extends org.specs2.specification.SpecificationInclusion { this: FragmentsBuilder =>
  override def include(f: Fragments): FragmentsFragment = super.include(addFragments(f))
}