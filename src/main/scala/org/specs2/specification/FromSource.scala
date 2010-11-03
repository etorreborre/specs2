package org.specs2
package specification

import io.FileReader._
import control.Exceptionx._
import control.LazyParameter
import control.LazyParameters._
import execute._
import matcher.MatchersImplicits._
import matcher._
import text.Trim._

private[specs2]
trait FromSource {
  val srcDir = "src/test/scala/"

  implicit def matchExample(expression: =>MatchResult[_]): Example = {
    createExample(exampleDescription, () => expression.toResult)
  }
  implicit def booleanExample(expression: =>Boolean): Example = {
    createExample(exampleDescription, () => toResult(expression))
  }
  implicit def resultExample(expression: =>execute.Result): Example = {
    createExample(exampleDescription, () => expression)
  }
  implicit def matchFragments(expression: =>MatchResult[_]): Fragments = {
    val desc = exampleDescription
    Fragments(createExample(desc, () => expression.toResult))
  }
  implicit def booleanFragments(expression: =>Boolean): Fragments = {
    val desc = exampleDescription
    Fragments(createExample(desc, () => toResult(expression)))
  }
  implicit def resultFragments(expression: =>Result): Fragments = {
    val desc = exampleDescription
    Fragments(createExample(desc, () => expression))
  }
  private def createExample(desc: String, body: () => Result) = new Example(desc, body)

  def exampleDescription: String = {
    val stackTrace = new Exception().getStackTrace()
    val trace = stackTrace.apply(4)
    val location = new Location(trace)
    val content = readLines(srcDir+location.path)
    content(location.lineNumber - 1).trimEnclosing("^", "^").trimEnclosing("{", "}")
  }
}