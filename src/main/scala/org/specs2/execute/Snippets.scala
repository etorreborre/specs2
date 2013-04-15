package org.specs2
package execute

import control.Exceptions._
import text.NotNullStrings._
import text.Trim._
import scala.reflect.macros.{Context => MContext}
import reflect.Macros._
import Snippet._
import control.LazyParameter

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
trait Snippets {

  def snippet[T](code: LazyParameter[T]) = macro Snippets.create[T]

  def createSnippet[T](expression: String, code: LazyParameter[T]) = new CodeSnippet[T](() => code.value, codeExpression = Some(expression))
}

object Snippets {
  def create[T](c: MContext)(code: c.Expr[LazyParameter[T]]): c.Expr[CodeSnippet[T]] = {
    import c.{universe => u}; import u._

    val result = c.Expr(methodCall(c)("createSnippet", stringExpr(c)(code), code.tree.duplicate))
    c.Expr(atPos(c.prefix.tree.pos)(result.tree))

  }
}

trait Snippet[T] {
  type ST <: Snippet[T]

  def cutMarker: String
  def cutMarkerFormat: String

  protected val code: () => T
  lazy val execute = code()
  protected  def asCode(s: String) = "```\n"+s.offset(offset)+"\n```"

  def mute: ST
  def eval: ST
  def offsetIs(n: Int = 0): ST

  def isMuted: Boolean
  def offset: Int
  def trimExpression: String => String

  def markdown(expression: String) = {
    val asCut = cut(codeExpression.getOrElse(expression))
    if (asCut.startsWith("\n")) ("\n\n"+asCode(asCut.removeStart("\n"))) else ("\n\n"+asCode(asCut))
  }

  protected def cut(expression: String) = {
    val text = trimExpression(expression)

    val splitted = text.split(cutMarkerFormat)

    splitted.zipWithIndex.
      collect { case (s, i) if i % 2 == 0 => s.removeStart("\n").removeEnd("\n") }.
      filter(_.trim.nonEmpty).mkString("\n")
  }

  protected def codeExpression: Option[String]

  def result: String = {
    val resultAsString = tryOr(execute.notNull)(e => e.getMessage)
    if (resultAsString == "()") ""
    else                        "> "+resultAsString
  }

  def resultMarkdown =
    if (result.isEmpty || isMuted) ""
    else                           asCode(result)

}

object Snippet {

  def trimSnippet = (expression: String) =>
    expression.removeFirst(s"$ls*\\{$ls*\n").
      removeLast(s"\\s*\\}$ls*")

  def trimEval = (s: String) => s.removeLast(s"(\\.)?$ls*eval$ls*")
  def trimOffsetIs = (s: String) => s.removeLast(s"\\s*\\}?(\\.)?$ls*offsetIs$parameters\\s*")

  val ls = "[ \t\\x0B\f]"
  val parameters = "(\\([^\\)]+\\))*"
}

case class CodeSnippet[T](code: () => T,
                          cutMarker: String = "\n// 8<--\n", cutMarkerFormat: String = s"$ls*// 8<\\-+.*\n",
                          trimExpression: String => String = trimSnippet,
                          codeExpression: Option[String] = None,
                          isMuted: Boolean = false, offset: Int = 0) extends Snippet[T] {
  type ST = CodeSnippet[T]

  def mute: ST = copy(isMuted = true)
  def eval: ST = copy(isMuted = false, trimExpression = trimEval andThen trimExpression)
  def offsetIs(n: Int = 0): ST = copy(offset = n, trimExpression = trimOffsetIs andThen trimExpression)

  def check[R : AsResult](verification: T => R) = CheckedSnippet[T, R](code, verification, cutMarker, cutMarkerFormat, trimExpression, codeExpression, isMuted, offset)
  def checkOk(implicit asResult: AsResult[T]) = CheckedSnippet[T, Result](code, (t: T) => AsResult(t), cutMarker, cutMarkerFormat, trimExpression, codeExpression, isMuted, offset)
}

case class CheckedSnippet[T, R : AsResult](code: () => T, verification: T => R,
                                           cutMarker: String = "\n// 8<--\n", cutMarkerFormat: String = s"\n$ls*// 8<\\-+.*\n",
                                           trimExpression: String => String = trimSnippet,
                                           codeExpression: Option[String] = None,
                                           isMuted: Boolean = false,  offset: Int = 0) extends Snippet[T] {
  type ST = CheckedSnippet[T, R]

  def mute: ST = copy(isMuted = true)
  def eval: ST = copy(isMuted = false, trimExpression = trimEval andThen trimExpression)
  def offsetIs(n: Int = 0): ST = copy(offset = n, trimExpression = trimOffsetIs andThen trimExpression)
  def verify = AsResult(verification(execute))
}
