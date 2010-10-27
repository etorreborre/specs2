package org.specs2
import specification._
import execute._
import matcher._
import control._

trait Specification extends BaseSpecification with MustExpectations
  with PredefinedFragments with StandardResults with Debug with AutoExamples
