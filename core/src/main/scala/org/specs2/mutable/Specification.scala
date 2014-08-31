package org.specs2
package mutable

import matcher._
import main.ArgumentsShortcuts
import org.specs2.execute.StandardResults
import specification.create.FormattingFragments
import specification.core.mutable.SpecificationStructure
import specification.mutable._
import specification.dsl.mutable._

/**
 * Class for a Specification using the mutable DSL and thrown expectations
 */
abstract class Specification extends SpecificationLike

trait SpecificationLike extends SpecificationStructure
  with SpecificationCreation
  with SpecificationFeatures

/**
 * Lightweight specification with only 3 implicit methods
 *
 *  - 2 implicits to create the specification string context
 *  - 1 implicit to create expectations with "must"
 *  - 1 implicit to add arguments to the specification
 *
 */
abstract class Spec extends SpecLike
trait SpecLike extends SpecificationStructure
  with ExampleDsl0
  with ArgumentsCreation
  with ArgumentsShortcuts
  with ActionDsl
  with MustThrownMatchers1
  with FormattingFragments
  with StandardResults


