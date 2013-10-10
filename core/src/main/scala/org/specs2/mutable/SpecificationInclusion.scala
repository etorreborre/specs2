package org.specs2
package mutable

import specification.{SpecificationStructure, Fragments, FragmentsFragment}

trait SpecificationInclusion extends org.specs2.specification.SpecificationInclusion { this: FragmentsBuilder =>
  override def include(f: Fragments): FragmentsFragment = super.include(addFragments(f.fragments))
  override def inline(specs: SpecificationStructure*): Fragments = addFragments(super.inline(specs:_*))
}