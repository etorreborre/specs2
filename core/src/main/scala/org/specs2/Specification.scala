package org.specs2

import matcher._
import main._
import specification.dsl.FragmentsDsl1
import specification._
import specification.core.ImmutableSpecificationStructure
import specification.create._

/**
 * Immutable Specification class
 *
 * It contains all necessary functionalities to create specifications:
 *
 *  - create fragments
 *  - create expectations
 */
abstract class Specification extends SpecificationLike

trait SpecificationLike extends ImmutableSpecificationStructure
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
trait SpecLike extends ImmutableSpecificationStructure
  with S2StringContext1
  with FragmentsDsl1
  with MustMatchers1
  with ArgumentsArgs
  with ArgumentsShortcuts
  with FormattingFragments

