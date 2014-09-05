package org.specs2
package specification
package dsl
package mutable

import main.ArgumentsShortcuts

trait SpecificationCreation extends specification.create.SpecificationCreation
  with AutoExamples
  with MutableDsl
  with ArgumentsShortcuts
  with ArgumentsDsl

