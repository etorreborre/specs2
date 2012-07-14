package org.specs2

import matcher.{MatchResult, Parameters, MatchersImplicits}
import specification._
import org.scalacheck.Prop
import MatchersImplicits._
/**
 * The ScalaCheck trait can be used to access ScalaCheck matchers
 */
trait ScalaCheck extends matcher.ScalaCheckMatchers with AutoExamples { this: FragmentsBuilder =>
  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def propFragmentsFragments(expression: =>Prop): FragmentsFragment = new FragmentsFragment(propFragments(expression))

  implicit def propFragments(expression: =>Prop)(implicit p: Parameters): Fragments = resultFragments(checkProperty(expression)(p))

  implicit def propExample(expression: =>Prop)(implicit p: Parameters) = resultExample(checkProperty(expression)(p))

  override implicit def aMatchResultExample(expression: =>MatchResult[_]): ToMatchResultExample2 =
    new ToMatchResultExample2(expression)
  class ToMatchResultExample2(ex: =>MatchResult[_]) extends ToMatchResultExample(ex) {
    override def eg = createExample(ex.toResult, 15)
  }
  override def eg(expression: =>MatchResult[_]): Example = createExample(expression.toResult)

  override implicit def aBooleanExample(expression: =>Boolean): ToBooleanExample2 = new ToBooleanExample2(expression)
  class ToBooleanExample2(ex: =>Boolean) extends ToBooleanExample(ex) {
    override def eg = createExample(toResult(ex), 15)
  }
  override def eg(expression: =>Boolean): Fragments = createExample(toResult(expression))

  override implicit def aResultExample(expression: =>execute.Result): ToResultExample2 = new ToResultExample2(expression)
  class ToResultExample2(ex: =>execute.Result) extends ToResultExample(ex) {
    override def eg = createExample(ex, 15)
  }
  override def eg(expression: =>execute.Result): Fragment = createExample(expression)

  private[specs2] override def createExample(expression: =>execute.Result, depth: Int = 16): Example =
    super.createExample(expression, depth)

  private[specs2] override def createExampleFragment(result: =>execute.Result, d: Int = 13, offset1: Int = -1,  offset2: Int = -1) =
    super.createExampleFragment(result, d+1, offset1-1, offset2-1)

  override private[specs2] def getDescription(depth: Int = 14, startOffset: Int = -1, endOffset: Int = -1) =
    super.getDescription(depth, startOffset, endOffset)

}
