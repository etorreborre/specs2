package org.specs2
package specification
package dsl
package mutable

import main.ArgumentsShortcuts
import org.specs2.specification.create.mutable.FormattingFragments

/**
 * Trait for creating a mutable specification
 */
trait SpecificationCreation extends specification.create.SpecificationCreation
  with FormattingFragments
  with AutoExamples
  with MutableDsl
  with ArgumentsShortcuts
  with ArgumentsDsl

