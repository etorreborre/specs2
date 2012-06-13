package org.specs2
package mutable

import specification.{Fragments, FragmentsFragment}

trait SpecificationInclusion extends org.specs2.specification.SpecificationInclusion { this: FragmentsBuilder =>
  override def include(f: Fragments): FragmentsFragment = super.include(addFragments(f.fragments))
}