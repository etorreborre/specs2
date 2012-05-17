package org.specs2
package mutable

import text.Trim._
import matcher.MatchResult
import matcher.MatchersImplicits._
import specification.Example


private [specs2]
trait AutoExamples extends org.specs2.specification.AutoExamples { this: specification.FragmentsBuilder =>

  override implicit def matchExample(expression: =>MatchResult[_]) : Example = createExample(expression.toResult, 12)
  override implicit def booleanExample(expression: =>Boolean)      : Example = createExample(toResult(expression), 12)
  override implicit def resultExample(expression: =>execute.Result): Example = createExample(expression, 12)

  override protected def getDescription(depth: Int = 12, startOffset: Int = -1, endOffset: Int = -1) =
    super.getDescription(depth, startOffset, endOffset)

  override private[specs2] def trimCode(code: String) = {
    List(";", "bt", "t", "endp", "br", "end", "p", ".", "eg", ".", ";").foldLeft(code.removeLast("\\(.*\\)"))(_.trim trimEnd _).
      trimFirst("eg").
      removeLast("\\(.*\\)").
      trimEnclosing("{", "}").
      trimEnclosing("`", "`").
      removeFirst("`\\(.*\\)").trimFirst("`")
  }

}
