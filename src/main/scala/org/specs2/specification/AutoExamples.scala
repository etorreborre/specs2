package org.specs2
package specification

import io.FromSource._
import control.LazyParameters._
import text.Trim._
import text._
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
private[specs2]
trait AutoExamples {

  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def matchFragmentsFragment(expression: =>MatchResult[_]): MatchResultFragment = {
    new MatchResultFragment(matchFragments(expression))
  }

  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def booleanFragmentsFragment(expression: =>Boolean): BooleanResultFragment =
    new BooleanResultFragment(booleanFragments(expression))

  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def resultFragmentsFragment(expression: =>Result): ResultFragment =
    new ResultFragment(resultFragments(expression))

  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def matchFragments(expression: =>MatchResult[_]) = {
    val desc = getSourceCode()
    Fragments.create(Example(CodeMarkup(desc), expression.toResult))
  }
  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def booleanFragments(expression: =>Boolean) = {
    val desc = getSourceCode()
    Fragments.create(Example(CodeMarkup(desc), toResult(expression)))
  }
  /** this implicit def is necessary when the expression is at the start of the spec */
  implicit def resultFragments(expression: =>Result) = {
    val desc = getSourceCode()
    Fragments.create(Example(CodeMarkup(desc), expression))
  }

  implicit def matchExample(expression: =>MatchResult[_]) = Example(CodeMarkup(getSourceCode()), expression.toResult)

  implicit def booleanExample(expression: =>Boolean) = Example(CodeMarkup(getSourceCode()), toResult(expression))

  implicit def resultExample(expression: =>execute.Result) = Example(CodeMarkup(getSourceCode()), expression)

  private[specs2] def getSourceCode(startDepth: Int = 6, endDepth: Int = 9, startLineOffset: Int = -1, endLineOffset: Int = -1): String = {
    val firstTry = getCodeFromTo(startDepth, endDepth, startLineOffset, endLineOffset)
    val code = firstTry match {
      case Right(c) => c
      case Left(e)  => getCodeFromTo(startDepth, startDepth, startLineOffset, endLineOffset) match { case Right(r) => r;  case Left(l) => e }
    }
    trimCode(code)
  }

  private[specs2] def trimCode(code: String) = {
    List("^", "t", "bt", "p", "br", "end", "endp", "end", "^").foldLeft(code)(_.trim trimEnd _).
    trimEnclosing("{", "}").
    trimEnclosing("`", "`").
    removeFirst("`\\(.*\\)").trimFirst("`")
  }

  class MatchResultFragment(fs: =>Fragments) extends FragmentsFragment(fs) {
    def ^[T](result: =>T)(implicit toResult: T => Result) = {
      val desc = getSourceCode(startLineOffset = 4, endLineOffset = 4)
      new FragmentsFragment(fs.add(Example(CodeMarkup(desc), toResult(result))))
    }
  }
  class BooleanResultFragment(fs: =>Fragments) extends FragmentsFragment(fs) {
    def ^[T](result: =>T)(implicit toResult: T => Result) = {
      val desc = getSourceCode()
      new FragmentsFragment(fs.add(Example(CodeMarkup(desc), toResult(result))))
    }
  }
  class ResultFragment(fs: =>Fragments) extends FragmentsFragment(fs) {
    def ^[T](result: =>T)(implicit toResult: T => Result) = {
      val desc = getSourceCode()
      new FragmentsFragment(fs.add(Example(CodeMarkup(desc), toResult(result))))
    }
  }
}

private[specs2]
object AutoExamples extends AutoExamples