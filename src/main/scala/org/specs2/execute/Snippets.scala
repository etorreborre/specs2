package org.specs2
package execute

import control.Exceptions._
import text.NotNullStrings._
import text.Trim._
import scala.reflect.macros.{Context => MContext}
import scala.reflect.runtime.universe._
import reflect.Macros
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

  def snippet[T](code: =>T): CodeSnippet[T] = macro Snippets.create[T]

  def createSnippet[T](expression: String, code: =>T): CodeSnippet[T] = new CodeSnippet[T](() => code.value, codeExpression = Some(expression)).mute

  def simpleName[T : WeakTypeTag]: String = implicitly[WeakTypeTag[T]].tpe.typeSymbol.name.toString.trim
  def fullName[T : WeakTypeTag]: String   = implicitly[WeakTypeTag[T]].tpe.typeSymbol.fullName.trim
  def termName(m: Any): String            = macro Macros.termName
}

object Snippets extends Snippets {
  def create[T](c: MContext)(code: c.Expr[T]): c.Expr[CodeSnippet[T]] = {
    import c.{universe => u}; import u._
    import Macros._
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

  def mute: ST
  def eval: ST
  def offsetIs(n: Int = 0): ST

  def isMuted: Boolean
  /** use the full snippet version + the cut one to determine what to display */
  def asCode: (String, String) => String
  def trimExpression: String => String

  def markdown(expression: String) = {
    val exp = codeExpression.getOrElse(expression)
    val asCut = cut(exp)
    if (asCut.startsWith("\n")) ("\n\n"+asCode(exp, asCut.removeStart("\n"))) else ("\n\n"+asCode(exp, asCut))
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
    if (isMuted || result.isEmpty) ""
    else                           asCode(result, result)

}

object Snippet {

  def trimSnippet = (expression: String) =>
    if (s"$ls*\\{$ls*\n.*".r.findPrefixOf(expression).isDefined) expression.removeFirst(s"$ls*\\{$ls*").removeLast(s"\n$ls*}")
    else                                                         expression

  def trimEval = (s: String) => s.removeLast(s"(\\.)?$ls*eval$ls*")
  def trimOffsetIs = (s: String) => s.removeLast(s"\\s*\\}?(\\.)?$ls*offsetIs$parameters\\s*")

  def markdownCode(offset: Int = 0) = (original: String, cut: String) =>
    if (original.contains("\n")) paragraphedCode(offset)(original, cut)
    else                         inlinedCode(cut)

  def paragraphedCode(offset: Int = 0) = (original: String, cut: String) => "```\n"+cut.offset(offset)+"\n```"
  def inlinedCode = (s: String) => "`"+s+"`"

  val ls = "[ \t\\x0B\f]"
  val parameters = "(\\([^\\)]+\\))*"
}

case class CodeSnippet[T](code: () => T,
                          cutMarker: String = "\n// 8<--\n", cutMarkerFormat: String = s"$ls*// 8<\\-+.*\n",
                          trimExpression: String => String = trimSnippet, asCode: (String, String) => String = markdownCode(0),
                          codeExpression: Option[String] = None,
                          isMuted: Boolean = false) extends Snippet[T] {
  type ST = CodeSnippet[T]

  def mute: ST = copy(isMuted = true)
  def eval: ST = copy(isMuted = false, trimExpression = trimEval andThen trimExpression)
  def offsetIs(n: Int = 0): ST = copy(trimExpression = trimOffsetIs andThen trimExpression, asCode = markdownCode(n))

  def check[R : AsResult](verification: T => R) = CheckedSnippet[T, R](code, verification, cutMarker, cutMarkerFormat, trimExpression, asCode, codeExpression, isMuted)
  def checkOk(implicit asResult: AsResult[T]) = CheckedSnippet[T, Result](code, (t: T) => AsResult(t), cutMarker, cutMarkerFormat, trimExpression, asCode, codeExpression, isMuted)
}

case class CheckedSnippet[T, R : AsResult](code: () => T, verification: T => R,
                                           cutMarker: String = "\n// 8<--\n", cutMarkerFormat: String = s"\n$ls*// 8<\\-+.*\n",
                                           trimExpression: String => String = trimSnippet, asCode: (String, String) => String = markdownCode(0),
                                           codeExpression: Option[String] = None,
                                           isMuted: Boolean = false) extends Snippet[T] {
  type ST = CheckedSnippet[T, R]

  def mute: ST = copy(isMuted = true)
  def eval: ST = copy(isMuted = false, trimExpression = trimEval andThen trimExpression)
  def offsetIs(n: Int = 0): ST = copy(trimExpression = trimOffsetIs andThen trimExpression, asCode = paragraphedCode(n))
  def verify = AsResult(verification(execute))
}
