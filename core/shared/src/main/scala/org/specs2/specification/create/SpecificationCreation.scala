package org.specs2
package specification
package create

import main.{ArgumentsShortcuts, ArgumentsArgs}
import org.specs2.specification.dsl._

trait SpecificationCreation extends
       AcceptanceDsl
  with AutoExamples
  with ArgumentsArgs
  with ArgumentsShortcuts
  with S2StringContext
  with FormattingFragments


