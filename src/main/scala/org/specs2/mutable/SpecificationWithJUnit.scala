package org.specs2
package mutable

import org.junit.runner._
import runner._
import specification.Example
import matcher.MatchResult

/**
 * This class must be inherited to allow a Specification to be executed as a JUnit test
 */
@RunWith(classOf[JUnitRunner])
abstract class SpecificationWithJUnit extends Specification {
  override lazy val exampleDepth = 11
  override implicit def matchExample(expression: =>MatchResult[_]) : Example = createExample(expression.toResult, exampleDepth)
  override implicit def booleanExample(expression: =>Boolean)      : Example = createExample(toResult(expression), exampleDepth)
  override implicit def resultExample(expression: =>execute.Result): Example = createExample(expression, exampleDepth)

  override private[specs2] def getDescription(depth: Int = exampleDepth, startOffset: Int = -1, endOffset: Int = -1) =
    super.getDescription(depth, startOffset, endOffset)
}
