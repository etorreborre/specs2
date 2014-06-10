package org.specs2
package specification
package create

import main.{ArgumentsShortcuts, ArgumentsArgs}
import specification.dsl.FragmentsDsl

trait SpecificationCreation extends
       FragmentsDsl
  with AutoExamples
  with ArgumentsArgs
  with ArgumentsShortcuts
  with S2StringContext
  with FormattingFragments


