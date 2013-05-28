package org.specs2
package mutable

import control._
import time._
import execute._
import matcher._
import main.ArgumentsShortcuts
import specification._
import control.Functions._

abstract class Specification extends SpecificationLike
trait SpecificationLike extends SpecificationStructure with SpecificationFeatures {
  def is = fragments
}

trait SpecificationFeatures extends mutable.FragmentsBuilder
   with SpecificationStringContext
   with mutable.SpecificationInclusion
   with ArgumentsArgs
   with ArgumentsShortcuts
   with MustThrownMatchers
   with ShouldThrownMatchers
   with FormattingFragments
   with StandardResults
   with StandardMatchResults
   with mutable.Tags
   with AutoExamples
   with TimeConversions
   with PendingUntilFixed
   with Contexts
   with SpecificationNavigation
   with ContextsInjection
   with Debug

