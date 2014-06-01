package org.specs2

import specification._
import specification.core.ImmutableSpecificationStructure
import specification.create.SpecificationCreation

abstract class Specification extends SpecificationLike

trait SpecificationLike extends ImmutableSpecificationStructure
  with SpecificationCreation
  with SpecificationFeatures

