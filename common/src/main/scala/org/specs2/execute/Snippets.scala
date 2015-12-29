package org.specs2
package execute

import control.Exceptions._
import text.NotNullStrings._
import text.Trim._
import scala.reflect.runtime.universe._
import reflect._
import Snippet._

/**
 * Snippets of code can be extracted from interpolated specification strings.
 *
 * When you want to specify that a piece of code must be included in the specification output, you can use the `snippet`
 * method to execute a this code and use the text in the output. If you just want to output part of the code you need to
 * delimit it with some comments `// 8<-------` (with as many dashes >= 2 as you want)
 *
 * Generally the value of a snippet will not be evaluated nor displayed but it is possible to show it using the `eval`
 * method on a Snippet.
 *
 * It is also possible to check that the result value is correct by using the `check` method.
 *
 */
trait Snippets {
  /** implicit parameters selected for the creation of Snippets */
  implicit def defaultSnippetParameters[T] = Snippet.defaultParams[T]

  /** implicit function modify the Snippet parameters */
  implicit class SettableSnippet[T](s: Snippet[T]) {
    def set(
       trimExpression: String => String   = defaultParams.trimExpression,
       cutter: String => String           = defaultParams.cutter,
       asCode: (String, String) => String = defaultParams.asCode,
       prompt: String => String           = defaultParams.prompt) =
      s.copy(params = s.params.copy(trimExpression = trimExpression,
                                    cutter = cutter,
                                    asCode = asCode,
                                    prompt = prompt))

    def promptIs(p: String) = s.copy(params = s.params.copy(prompt = simplePrompt(p)))
    def offsetIs(offset: Int) = s.copy(params = s.params.offsetIs(offset))
    def eval = s.copy(params = s.params.eval)
    def check[R : AsResult](f: T => R) = s.copy(params = s.params.check(f))
  }

  implicit class SettableSnippet1[T : AsResult](s: Snippet[T]) {
    def checkOk = s.copy(params = s.params.copy(verify = Some((t: T) => AsResult(t))))
  }

  def snippet[T](code: =>T)(implicit params: SnippetParams[T]): Snippet[T] = macro Snippets.create[T]

  def createSnippet[T](rangepos: Boolean, expression: String, code: =>T, params: SnippetParams[T]): Snippet[T] = {
    if (rangepos) new Snippet[T](() => code, codeExpression = Some(expression), params.copy(trimExpression = trimRangePosSnippet))
    else          new Snippet[T](() => code, codeExpression = None, params)
  }

  def simpleName[T : WeakTypeTag]: String = implicitly[WeakTypeTag[T]].tpe.typeSymbol.name.toString.trim
  def fullName[T : WeakTypeTag]: String   = implicitly[WeakTypeTag[T]].tpe.typeSymbol.fullName.trim
  def termName(m: Any): String            = macro Macros.termName
}

import reflect.MacroContext._

object Snippets extends Snippets {
  def create[T](c: Context)(code: c.Expr[T])(params: c.Expr[SnippetParams[T]]): c.Expr[Snippet[T]] = {
    import c.{universe => u}; import u._
    import Macros._
    val result = c.Expr(methodCall(c)("createSnippet", q"${c.macroApplication.pos.isRange}", stringExprMacroPos(c)(code), code.tree.duplicate, params.tree))
    c.Expr(atPos(c.prefix.tree.pos)(result.tree))
  }
}

/**
 * Captured snippet of code with: a value of type T, a string representing the expression, captured by a macro,
 * some evaluation and display parameters
 */
case class Snippet[T](code: () => T,
                      codeExpression: Option[String] = None,
                      params: SnippetParams[T] = SnippetParams[T]()) {

  lazy val execute = code()

  def mustBeVerified = params.verify.isDefined

  /**
   * show the snippet either taking the expression captured by the macro or another expression passed by the caller
   * (for example in an interpolated s2 string when -Yrangepos is disabled)
   */
  def show(expression: String = "") = {
    val exp     = codeExpression.getOrElse(expression)
    val trimmed = params.trimExpression(exp)
    val cut     = params.cutter(trimmed)
    params.asCode(exp, cut)
  }

  lazy val result: String = {
    val resultAsString = tryOr(execute.notNull)(e => e.getMessage.notNull)
    if (resultAsString == "()") ""
    else                        params.prompt(resultAsString)
  }

  lazy val showResult =
    if (!params.evalCode || result.isEmpty) ""
    else                                    params.asCode(result, result)

  def verify = ResultExecution.execute(params.verify.map(f => f(execute)).getOrElse(Success()))
}

/**
 * Evaluation and display parameters for a Snippet.
 *
 * It is possible to change:
 *
 *  - the function that's trimming the expression from newlines or accolades
 *  - the `cutter` function to remove part which must not be shown
 *  - the `asCode` function to render the resulting text
 *  - the `prompt` function to possibly display the evaluated result with a prompt
 *  - the `eval` boolean indicating if a snippet must be evaluated
 *  - the `verify` function checking the result
 */
case class SnippetParams[T]( 
  trimExpression: String => String   = trimApproximatedSnippet,
  cutter: String => String           = ScissorsCutter(),
  asCode: (String, String) => String = markdownCode(offset = 0),
  prompt: String => String           = greaterThanPrompt,
  evalCode: Boolean                  = false,
  verify: Option[T => Result]        = None) {
  def offsetIs(offset: Int) = copy(asCode = markdownCode(offset = offset))
  def eval = copy(evalCode = true)
  def check[R : AsResult](f: T => R) = copy(verify = Some((t: T) => AsResult(f(t))))
}
/**
 * Implementation of a function to cut pieces of code by using some comments as markers
 */
case class ScissorsCutter(cutMarker: String       = scissorsMarker,
                          cutMarkerFormat: String = scissorsMarkerFormat) extends (String => String) {
  def apply(text: String) = {
    val splitted = text.split(cutMarkerFormat)

    splitted.zipWithIndex.
      collect { case (s, i) if i % 2 == 0 => s.removeStart("\n").removeEnd("\n") }.
      filter(_.trim.nonEmpty).mkString("\n")
  }
}

object Snippet {

  def defaultParams[T] = SnippetParams[T]()

  lazy val scissorsMarker       = "\n// 8<--\n"
  lazy val scissorsMarkerFormat = s"$ls*// 8<\\-+.*\n"

  /**
   * RangePos snippets are built by using all the string at the macro application point
   * (it used to be only the "inside" code but that got broken in 2.11.0-M8)
   *
   * 3 things need to be trimmed:
   *
   *  - the "snippet" method call
   *
   *  - the expression from start and end accolades if it is a multiline expression
   *
   *  - in some case it is necessary to add a dummy expression so that the range position of the captured snippet is
   * correct. /**/;1/**/ is the smallest such expression
   */
  def trimRangePosSnippet = (call: String) => {

    val expression = Trimmed(call).removeStart("snippet")
    val trimmed =
      if (s"$ls*\\{$ls*.*".r.findPrefixOf(expression).isDefined) Trimmed(Trimmed(expression).removeFirst(s"\\{")).removeLast(s"\\}")
      else                                                       expression

    Trimmed(trimmed).removeAll("/**/;1/**/").trim
  }

  /**
   * trim the expression from start and end accolades if it is a multiline expression
   * + in some case it is necessary to add a dummy expression so that the range position of the captured snippet is
   * correct. /**/;1/**/ is the smallest such expression
   */
  def trimApproximatedSnippet = (expression: String) => {
    expression.removeFirst(s"snippet\\s*\\{").removeLast(s"\\}").
      removeLast("(\\.(offsetIs|promptIs|set)"+parameters+")+").
      removeLast("(\\.(eval|check|checkOk))+").
      trim
  }

  /** display a cut piece of code as markdown depending on the existence of newlines in the original piece */
  def markdownCode(multilineQuotes: String => String = defaultMultilineMarkdownQuotes, singleLineQuotes: String => String = defaultSingleLineQuotes, offset: Int = 0) = (original: String, cut: String) => {
    if (original.startsWith("\n"))    "\n\n"+multilineQuotes(cut.removeStart("\n").offset(offset))
    else if (original.contains("\n")) "\n\n"+multilineQuotes(cut.offset(offset))+"\n"
    else                              singleLineQuotes(cut)
  }

  def defaultMultilineMarkdownQuotes = (s: String) => s"```\n$s\n```"
  def githubMultilineMarkdownQuotes = (s: String) => s"```scala\n$s\n```"
  def defaultSingleLineQuotes = (s: String) => s"`$s`"

  def greaterThanPrompt = simplePrompt("> ")
  def simplePrompt(p: String) = (s: String) => p+s
  def emptyPrompt  = (s: String) => s

  lazy val ls = "[ \t\\x0B\f]"
  lazy val parameters = "(\\([^\\)]+\\))*"
}

