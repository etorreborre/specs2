package org.specs2
package mutable

import text.Trim._
import matcher.MatchResult
import matcher.MatchersImplicits._
import specification.Example
import io.FromSource._

private [specs2]
trait AutoExamples extends org.specs2.specification.AutoExamples { this: specification.FragmentsBuilder =>
  override lazy val exampleDepth = 12

  override implicit def matchExample(expression: =>MatchResult[_]) : Example = createExample(expression.toResult, exampleDepth)
  override implicit def booleanExample(expression: =>Boolean)      : Example = createExample(toResult(expression), exampleDepth)
  override implicit def resultExample(expression: =>execute.Result): Example = createExample(expression, exampleDepth)

  override private[specs2]  def getDescription(depth: Int = exampleDepth, startOffset: Int = -1, endOffset: Int = -1) =
    super.getDescription(depth, startOffset, endOffset)

  override private[specs2] def getSourceCode(startDepth: Int = 9, endDepth: Int = 12, startLineOffset: Int = -1, endLineOffset: Int = -1): String = {
    val code = getCodeFromTo(startDepth, startDepth, startLineOffset)
    val example = code match {
      case Right(c) if c matches ".*eg.*\\{.*" => getExampleFrom(startDepth, startLineOffset)
      case Right(c) if c matches ".*\\}.*eg.*" => getExampleTo(startDepth, startLineOffset)
      case other                               => other
    }
    example match {
      case Right(c) => trimCode(c)
      case Left(e)  => e
    }
  }


  override private[specs2] def trimCode(code: String) = {
    List(";", "bt", "t", "endp", "br", "end", "p", ".", "eg", ".", ";").foldLeft(code)(_.trim trimEnd _).
      trimFirst("eg").
      trimEnclosing("{", "}").
      trimEnclosing("`", "`").
      removeFirst("`\\(.*\\)").trimFirst("`")
  }

}
