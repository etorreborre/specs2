package org.specs2

import control._
import execute._
import matcher._
import specification._

trait Specification extends BaseSpecification with MustExpectations
  with PredefinedFragments with StandardResults with Debug with AutoExamples
