package org.specs2
package specification
package create

import main.{ArgumentsShortcuts, ArgumentsArgs}
import specification.dsl.FragmentDsl

trait SpecificationCreation extends
       FragmentDsl
  with AutoExamples
  with ArgumentsArgs
  with ArgumentsShortcuts
  with S2StringContext
  with FormattingFragments


