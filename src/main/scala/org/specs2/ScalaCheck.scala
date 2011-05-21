package org.specs2

import matcher.{Parameters, MatchResult}
import specification.{AutoExamples, Example, Fragments}
import text.CodeMarkup
import org.scalacheck.Prop
/**
 * The ScalaCheck trait can be used to access ScalaCheck matchers
 */
trait ScalaCheck extends matcher.ScalaCheckMatchers with AutoExamples {
  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def propFragments(expression: =>Prop)(implicit p: Parameters): Fragments = {
    val desc = code()
    Fragments.create(Example(CodeMarkup(desc), checkProperty(expression)(p)))
  }

}
