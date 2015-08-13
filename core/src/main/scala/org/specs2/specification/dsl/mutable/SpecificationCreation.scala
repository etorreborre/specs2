package org.specs2
package specification
package dsl
package mutable

import main.ArgumentsShortcuts
import org.specs2.specification.core.Fragments
import org.specs2.specification.create.InterpolatedFragment
import org.specs2.specification.create.mutable.FormattingFragments

/**
 * Trait for creating a mutable specification
 */
trait SpecificationCreation extends specification.create.SpecificationCreation
  with FormattingFragments
  with AutoExamples
  with MutableDsl
  with ArgumentsShortcuts
  with ArgumentsDsl {

  /** add fragments created with the s2 interpolated string */
  override def s2(content: String, Yrangepos: Boolean, texts: Seq[String],
                  textsStartPositions: Seq[String], textsEndPositions: Seq[String],
                  variables: Seq[InterpolatedFragment], rangeExpressions: Seq[String]): Fragments = {
    addFragments(super.s2(
      content, Yrangepos, texts,
      textsStartPositions, textsEndPositions,
      variables, rangeExpressions).append(fragmentFactory.break))
  }


}

