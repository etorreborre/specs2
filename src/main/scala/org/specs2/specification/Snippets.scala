package org.specs2
package specification

import control.Exceptions._
import text.NotNullStrings._
import text.Trim._
import execute.{Result, AsResult}
import matcher.ResultMatchers
import main.{ArgumentsArgs, Arguments}
import Snippet._
/**
 * Snippets of code can be extracted from interpolated specification strings.
 *
 * When you want to specify that a piece of code must be included in the specification output, you can use the `snippet`
 * method to execute a this code and use the text in the output. If you just want to output part of the code you need to
 * delimit it with some comments `// 8<-------` (with as many dashes as you want)
 *
 * Generally the last value of a snippet will be displayed separately but it is possible to avoid this by using the `mute`
 * method on a Snippet.
 *
 * It is also possible to check that the result value is equal to a specific value by using the `check[R : AsResult](f: T => R)` method.
 *
 */
trait Snippets { outer: SpecificationStringContext with FragmentsBuilder with ArgumentsArgs =>

  def `8<--`(offset: Int = 0): CodeSnippet[Unit] =
    CodeSnippet(code = () => (), cutMarker = "\n`8<--`\n", cutMarkerFormat = s"\n.*`8\\<\\-\\-`$parameters.*\n").offsetIs(offset)
  def `8<--`: CodeSnippet[Unit] =
    CodeSnippet(code = () => (), cutMarker = "\n`8<--`\n", cutMarkerFormat = "\n.*`8\\<\\-\\-`.*\n")

  def cutHere(offset: Int = 0): CodeSnippet[Unit] =
    CodeSnippet(code = () => (), cutMarker = "\ncutHere\n", cutMarkerFormat = "\n.*cutHere.*\n").offsetIs(offset)
  def cutHere: CodeSnippet[Unit] =
    CodeSnippet(code = () => (), cutMarker = "\ncutHere\n", cutMarkerFormat = s"\n.*cutHere$parameters.*\n")

  implicit class cutResult[T](t: =>T) {
    def eval[S](snippet: CodeSnippet[S]) =
      snippet.copy(code = () => t, trimExpression = snippet.trimExpression andThen trimEval).eval
  }
  def snippet[T](code: =>T) = CodeSnippet(() => code).mute

  implicit def snippetIsSpecPart[T](snippet: Snippet[T]): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = text ^ snippet.fragments(expression)
  }
}

trait Snippet[T] {
  type ST <: Snippet[T]

  def cutMarker: String
  def cutMarkerFormat: String

  def fragments(expression: String) =
    Fragments.createList(Text(asCode(cut(expression)))).add(resultFragments)

  protected val code: () => T
  protected lazy val execute = code()
  protected  def asCode(s: String) =
    "```\n"+s.offset(offset)+"\n```"

  def mute: ST
  def eval: ST
  def offsetIs(n: Int = 0): ST

  def isMuted: Boolean
  def offset: Int

  protected def resultFragments = {
    if (result.isEmpty || isMuted) Fragments.createList()
    else                           Fragments.createList(Text(asCode(result)))
  }

  protected def cut(expression: String) = {
    val text = trimExpression(expression)

    val splitted = text.split(cutMarkerFormat)

    splitted.zipWithIndex.
      collect { case (s, i) if i % 2 == 0 => s.removeStart("\n").removeEnd("\n") }.
      filter(_.trim.nonEmpty).mkString("\n")
  }

  def trimExpression: String => String

  protected def result: String = {
    val resultAsString = tryOr(execute.notNull)(e => e.getMessage)
    if (resultAsString == "()") ""
    else                        "> "+resultAsString
  }
}

object Snippet {

  def trimSnippet = (expression: String) =>
    expression.removeFirst(s"\\s*snippet$ls*\\{$ls*\n").
      removeLast("\\s*\\}\\s*")

  def trimEval = (s: String) => s.removeLast(s"(\\.)?$ls*eval$ls*")
  def trimOffsetIs = (s: String) => s.removeLast(s"\\s*\\}?(\\.)?$ls*offsetIs$parameters\\s*")

  val ls = "[ \t\\x0B\f]"
  val parameters = "(\\([^\\)]+\\))*"
}

case class CodeSnippet[T](code: () => T,
                          cutMarker: String = "\n// 8<--\n", cutMarkerFormat: String = s"$ls*// 8<\\-+.*\n",
                          trimExpression: String => String = trimSnippet,
                          isMuted: Boolean = false, offset: Int = 0) extends Snippet[T] {
  type ST = CodeSnippet[T]

  def mute: ST = copy(isMuted = true)
  def eval: ST = copy(isMuted = false, trimExpression = trimEval andThen trimExpression)
  def offsetIs(n: Int = 0): ST = copy(offset = n, trimExpression = trimOffsetIs andThen trimExpression)

  def check[R : AsResult](verification: T => R) = CheckedSnippet[T, R](code, verification, cutMarker, cutMarkerFormat, trimExpression, isMuted, offset)
  def checkOk(implicit asResult: AsResult[T]) = CheckedSnippet[T, Result](code, (t: T) => AsResult(t), cutMarker, cutMarkerFormat, trimExpression, isMuted, offset)
}

case class CheckedSnippet[T, R : AsResult](code: () => T, verification: T => R,
                                           cutMarker: String = "\n// 8<--\n", cutMarkerFormat: String = s"\n$ls*// 8<\\-+.*\n",
                                           trimExpression: String => String = trimSnippet,
                                           isMuted: Boolean = false,  offset: Int = 0) extends Snippet[T] {
  type ST = CheckedSnippet[T, R]

  def mute: ST = copy(isMuted = true)
  def eval: ST = copy(isMuted = false, trimExpression = trimEval andThen trimExpression)
  def offsetIs(n: Int = 0): ST = copy(offset = n, trimExpression = trimOffsetIs andThen trimExpression)

  override def fragments(expression: String) =
    Fragments.createList(Text(asCode(cut(expression))), Step(AsResult(verification(execute)).mapMessage("Snippet failure: "+_)))
}

trait SpecificationExecution extends ResultMatchers { this: Specification =>
  def executeSpec(s: SpecificationStructure)       = FragmentExecution.executeExamplesResult(s.content)(Arguments())
  def executionMustBeOk(s: SpecificationStructure) = executeSpec(s) must beSuccessful
}

