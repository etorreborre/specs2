package org.specs2
package specification

import control.Exceptions._
import text.NotNullStrings._
import text.Trim._
import execute.AsResult
import matcher.ResultMatchers
import main.{ArgumentsArgs, Arguments}

/**
 * Snippets of code can be extracted from interpolated specification strings.
 *
 * When you want to specify that a piece of code must be included in the specification output, you can use the `snippet`
 * method to execute a this code and use the text in the output. If you just want to output part of the code you need to
 * delimit it with some comments `// 8<-------` (with as many dashes as you want
 *
 * Generally the last value of a snippet will be displayed separately but it is possible to avoid this by using the `mute`
 * method on a Snippet.
 *
 * It is also possible to check that the result value is equal to a specific value by using the `check[R : AsResult](f: T => R)` method.
 *
 */
trait Snippets { this: SpecificationStringContext with FragmentsBuilder with ArgumentsArgs =>
  def snippet[T](code: =>T) = CodeSnippet(() => code).mute

  implicit def snippetIsSpecPart[T](snippet: Snippet[T]): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = text ^ snippet.fragments(expression)
  }
}

trait Snippet[T] {
  type ST <: Snippet[T]

  lazy val cutComment = "// 8<\\-+"

  def fragments(expression: String) =
    Fragments.createList(Text(asCode(cut(expression)))).add(resultFragments)

  protected val code: () => T
  protected lazy val execute = code()
  protected  def asCode(s: String) = "```scala\n"+s+"\n```"

  def mute: ST = set(muted = true)
  def eval: ST = set(muted = false)
  def set(muted: Boolean = true): ST
  def isMuted: Boolean

  protected def resultFragments = {
    if (result.isEmpty || isMuted) Fragments.createList()
    else                           Fragments.createList(Text(asCode(result)))
  }

  protected def cut(expression: String) = {
    val text = if (expression.contains("// 8<-")) expression else ("// 8<--" + trimSnippet(expression) + "// 8<--")
    val splitted = text.split(cutComment)
    splitted.zipWithIndex.
           collect { case (s, i) if i % 2 == 1 => s }.
           filter(_.nonEmpty).mkString.
           removeStart("\n")
  }

  private def trimSnippet(expression: String) =
    expression.trimStart("snippet").
      removeFirst("\\s*\\{\n*").
      removeLast("\\}")

  protected def result: String = {
    val resultAsString = tryOr(execute.notNull)(e => e.getMessage)
    if (resultAsString == "()") ""
    else                        "> "+resultAsString
  }
}

case class CodeSnippet[T](code: () => T, isMuted: Boolean = false) extends Snippet[T] {
  type ST = CodeSnippet[T]
  def set(muted: Boolean = true): ST = copy(isMuted = muted)
  def check[R : AsResult](verification: T => R) = CheckedSnippet[T, R](code, verification)
}

case class CheckedSnippet[T, R : AsResult](code: () => T, verification: T => R, isMuted: Boolean = false) extends Snippet[T] {
  type ST = CheckedSnippet[T, R]

  def set(muted: Boolean = true): ST = copy(isMuted = muted)

  override def fragments(expression: String) =
    Fragments.createList(Text(asCode(cut(expression))), Step(AsResult(verification(execute)).mapMessage("Snippet failure: "+_)))
}

trait SpecificationExecution extends ResultMatchers { this: Specification =>
  def executeSpec(s: SpecificationStructure)       = FragmentExecution.executeExamplesResult(s.content)(Arguments())
  def executionMustBeOk(s: SpecificationStructure) = executeSpec(s) must beSuccessful
}

