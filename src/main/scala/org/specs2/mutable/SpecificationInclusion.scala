package org.specs2
package mutable

import main.Arguments
import specification.{Fragments, SpecificationStructure}

trait SpecificationInclusion extends org.specs2.specification.SpecificationInclusion { this: FragmentsBuilder =>
  override def include(f: Fragments): FragmentsFragment = super.include(addFragments(f))
  override def include(s: SpecificationStructure): FragmentsFragment = include(s.content)
  override def include(args: Arguments, s: SpecificationStructure) = include(s.content.overrideArgs(args))
}