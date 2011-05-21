package org.specs2
package mutable

import control._
import time._
import execute._
import matcher._
import specification.{SpecificationStructure, FormattingFragments, AutoExamples}
import main.ArgumentsShortcuts

trait Specification extends SpecificationStructure with SpecificationFeatures {
  def is = specFragments
}

trait SpecificationFeatures extends FragmentsBuilder
   with SpecificationInclusion
   with ArgumentsArgs
   with ArgumentsShortcuts
   with MustThrownMatchers
   with ShouldThrownMatchers
   with FormattingFragments
   with StandardResults
   with StandardMatchResults
   with AutoExamples
   with TimeConversions
   with PendingUntilFixed
   with Debug
