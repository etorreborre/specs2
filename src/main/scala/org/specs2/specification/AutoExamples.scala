package org.specs2
package specification

import io.FromSource._
import control.LazyParameters._
import execute._
import matcher.MatchersImplicits._
import matcher._

/**
 * Create example descriptions by reading the corresponding line in the source file.
 * 
 * { 1 must_== 1 } ^
 *                 end
 *                 
 * will be simply reported as 
 * 
 * + 1 must_== 1
 *                 
 * * An important limitation is that the example code must fit on one line only*
 * 
 * The source dir is assumed to be "src/test/scala/" by default but this can be modified
 * by setting the "specs2.srcDir" System property
 *
 * This trait provides implicit definitions to create examples from:
 *  * boolean expressions
 *  * match results
 *  * results 
 * 
 */
trait AutoExamples {
  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def matchFragments(expression: =>MatchResult[_]): Fragments = {
    val desc = getCode()
    Fragments(Example(desc, expression.toResult))
  }
  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def booleanFragments(expression: =>Boolean): Fragments = {
    val desc = getCode()
    Fragments(Example(desc, toResult(expression)))
  }
  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def resultFragments(expression: =>Result): Fragments = {
    val desc = getCode()
    Fragments(Example(desc, expression))
  }
  implicit def matchExample(expression: =>MatchResult[_]): Example = {
    Example(getCode(), expression.toResult)
  }
  implicit def booleanExample(expression: =>Boolean): Example = {
    Example(getCode(), toResult(expression))
  }
  implicit def resultExample(expression: =>execute.Result): Example = {
    Example(getCode(), expression)
  }
}
private[specs2]
object AutoExamples extends AutoExamples