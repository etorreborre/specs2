package org.specs2

import matcher.Parameters
import specification._
import org.scalacheck.Prop
/**
 * The ScalaCheck trait can be used to access ScalaCheck matchers
 */
trait ScalaCheck extends matcher.ScalaCheckMatchers with AutoExamples { this: FragmentsBuilder =>
  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def propFragmentsFragments(expression: =>Prop): FragmentsFragment = new FragmentsFragment(propFragments(expression))

  implicit def propFragments(expression: =>Prop)(implicit p: Parameters): Fragments = resultFragments(checkProperty(expression)(p))

  implicit def propExample(expression: =>Prop)(implicit p: Parameters) = resultExample(checkProperty(expression)(p))

  private[specs2] override def createExampleFragment(result: =>execute.Result, d: Int = 13, offset1: Int = -1,  offset2: Int = -1) =
    super.createExampleFragment(result, d+1, offset1-1, offset2-1)

  override protected def getDescription(depth: Int = 13, startOffset: Int = -1, endOffset: Int = -1) =
    super.getDescription(depth, startOffset, endOffset)

}
