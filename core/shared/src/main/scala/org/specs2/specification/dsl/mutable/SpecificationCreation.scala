package org.specs2
package specification
package dsl
package mutable

import main.ArgumentsShortcuts
import org.specs2.specification.core.Fragments
import org.specs2.specification.create.mutable.FormattingFragments

/**
 * Trait for creating a mutable specification
 */
trait SpecificationCreation extends specification.create.SpecificationCreation
  with FormattingFragments
  with AutoExamples
  with MutableDsl
  with ArgumentsShortcuts
  with ArgumentsDsl:

  /** add fragments created with the s2 interpolated string */
  override def postProcessS2Fragments(fs: Fragments): Fragments =
    addFragments(fs.append(fragmentFactory.break))


