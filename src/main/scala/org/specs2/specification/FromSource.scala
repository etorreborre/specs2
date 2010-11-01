package org.specs2
package specification

import io.FileReader._
import control.Exceptionx._
import control.LazyParameter
import matcher.MatchersImplicits._
import text.Trim._

private[specs2]
trait FromSource {
  val srcDir = "src/test/scala/"
  implicit def matchExample(expression: =>matcher.MatchResult[_]): Example = {
    new Example(exampleDescription, () => expression.toResult)
  }
  implicit def booleanExample(expression: =>Boolean): Example = {
    new Example(exampleDescription, () => toResult(expression))
  }
  implicit def resultExample(expression: =>execute.Result): Example = {
    new Example(exampleDescription, () => expression)
  }
  def exampleDescription: String = {
    val stackTrace = new Exception().getStackTrace()
    val trace = stackTrace.apply(4)
    val location = new Location(trace)
    val content = readLines(srcDir+location.path)
    content(location.lineNumber-1).trimEnclosing("^", "^").trimEnclosing("{", "}")
  }
}