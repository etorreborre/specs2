package org.specs2

import matcher.Parameters
import specification._
import text.CodeMarkup
import org.scalacheck.Prop
/**
 * The ScalaCheck trait can be used to access ScalaCheck matchers
 */
trait ScalaCheck extends matcher.ScalaCheckMatchers with AutoExamples { this: FragmentsBuilder =>
  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def propFragmentsFragments(expression: =>Prop): FragmentsFragment = new FragmentsFragment(propFragments(expression))

  implicit def propFragments(expression: =>Prop)(implicit p: Parameters): Fragments = resultFragments(checkProperty(expression)(p))

  implicit def propExample(expression: =>Prop)(implicit p: Parameters) = resultExample(checkProperty(expression)(p))

}
