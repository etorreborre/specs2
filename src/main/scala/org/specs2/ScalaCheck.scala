package org.specs2

import matcher.{Parameters, MatchResult}
import specification._
import text.CodeMarkup
import org.scalacheck.Prop
/**
 * The ScalaCheck trait can be used to access ScalaCheck matchers
 */
trait ScalaCheck extends matcher.ScalaCheckMatchers with AutoExamples {
  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def propFragmentsFragments(expression: =>Prop): FragmentsFragment = new FragmentsFragment(propFragments(expression))

  implicit def propFragments(expression: =>Prop)(implicit p: Parameters): Fragments = {
    val desc = getSourceCode(startDepth = 10, endDepth = 13, startLineOffset = 0, endLineOffset = 0)
    Fragments.create(Example(CodeMarkup(desc), checkProperty(expression)(p)))
  }

  implicit def propExample(expression: =>Prop)(implicit p: Parameters) = Example(CodeMarkup(getSourceCode()),  checkProperty(expression)(p) )

}
