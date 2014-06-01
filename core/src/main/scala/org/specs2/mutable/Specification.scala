package org.specs2
package mutable

import specification.core.mutable.SpecificationStructure
import specification.mutable._
import specification.dsl.mutable.SpecificationCreation

/**
 * Class for a Specification using the mutable DSL and thrown expectations
 */
abstract class Specification extends SpecificationLike

trait SpecificationLike extends SpecificationStructure
  with SpecificationCreation
  with SpecificationFeatures
