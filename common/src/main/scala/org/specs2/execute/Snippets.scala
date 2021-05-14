package org.specs2
package execute

import control.Exceptions.*
import text.NotNullStrings.*
import text.Trim.*
import Snippet.*
import scala.quoted.*

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
trait Snippets:
  /** implicit parameters selected for the creation of Snippets */
  given defaultSnippetParameters[T]: SnippetParams[T] =
    Snippet.defaultParams[T]

  /** implicit function modify the Snippet parameters */
  extension [T](s: Snippet[T])

    def set(
       trimExpression: String => String   = defaultParams.trimExpression,
       cutter: String => String           = defaultParams.cutter,
       asCode: (String, String) => String = defaultParams.asCode,
       prompt: String => String           = defaultParams.prompt): Snippet[T] =
      s.copy(params = s.params.copy(trimExpression = trimExpression,
                                    cutter = cutter,
                                    asCode = asCode,
                                    prompt = prompt))

    def promptIs(p: String): Snippet[T] =
      s.copy(params = s.params.copy(prompt = simplePrompt(p)))

    def offsetIs(offset: Int): Snippet[T] =
      s.copy(params = s.params.offsetIs(offset))

    def eval: Snippet[T] =
      s.copy(params = s.params.eval)


  extension [T, R : AsResult](s: Snippet[T])
    def check(f: T => R): Snippet[T] =
      s.copy(params = s.params.check(f))

  extension [T : AsResult](s: Snippet[T])
    def checkOk: Snippet[T] =
      s.copy(params = s.params.copy(verify = Some((t: T) => AsResult(t))))

  inline def snippet[T](inline code: =>T)(using params: SnippetParams[T]): Snippet[T] =
    ${ Snippets.create[T]('{() => code}, 'params) }

  inline def simpleName[T]: String =
    ${ Snippets.typeSimpleName[T] }

  inline def fullName[T]: String =
    ${ Snippets.typeFullName[T] }

  inline def termName[T](inline t: =>T): String =
    ${ Snippets.termFullName[T]('t) }


object Snippets extends Snippets:

  def create[T](code: Expr[() => T], params: Expr[SnippetParams[T]])(using quotes: Quotes)(using t: Type[T], t1: Type[() => T]): Expr[Snippet[T]] =
    import quotes.reflect.*
    val expression = Expr(Position.ofMacroExpansion.sourceCode.getOrElse("no source code found"))
    // we need to pass () => T here because betaReduce would evaluate the code here otherwise
    Expr.betaReduce('{createSnippet[t.Underlying]($expression, $code, $params)})

  def createSnippet[T](expression: String, code: () => T, params: SnippetParams[T]): Snippet[T] =
    new Snippet[T](code, codeExpression = Some(expression), params)

  def typeSimpleName[T](using quotes: Quotes)(using t: Type[T]): Expr[String] =
    import quotes.reflect.*
    Expr(TypeTree.of[T].symbol.name)

  def typeFullName[T](using quotes: Quotes)(using t: Type[T]): Expr[String] =
    import quotes.reflect.*
    Expr(TypeTree.of[T].symbol.fullName)

  def termFullName[T](e: Expr[T])(using quotes: Quotes): Expr[String] =
    import quotes.reflect.*
    val name = e.asTerm match
      case Ident(termName)                                    => termName
      case Select(_, termName)                                => termName.toString
      case Inlined(_,_,Apply(Ident(termName),_))              => termName.toString
      case Inlined(_,_,Apply(TypeApply(Ident(termName),_),_)) => termName.toString
      case Inlined(_,_,Select(_,termName))                    => termName.toString
      case other                                              => report.error("The code must be a member selection, or a function application", e);""
    Expr(name)


/**
 * Captured snippet of code with: a value of type T, a string representing the expression, captured by a macro,
 * some evaluation and display parameters
 */
case class Snippet[T](code: () => T,
                      codeExpression: Option[String] = None,
                      params: SnippetParams[T] = SnippetParams[T]()):

  lazy val execute = code()

  def mustBeVerified = params.verify.isDefined

  /**
   * show the snippet either taking the expression captured by the macro or another expression passed by the caller
   * (for example in an interpolated s2 string when -Yrangepos is disabled)
   */
  def show(expression: String = "") =
    val exp     = codeExpression.getOrElse(expression)
    val trimmed = params.trimExpression(exp)
    val cut     = params.cutter(trimmed)
    params.asCode(exp, cut)

  lazy val result: String =
    val resultAsString = tryOr(execute.notNull)(e => e.getMessage.notNull)
    if resultAsString == "()" then ""
    else                        params.prompt(resultAsString)

  lazy val showResult =
    if !params.evalCode || result.isEmpty then ""
    else                                    params.asCode(result, result)

  def verify = ResultExecution.execute(params.verify.map(f => f(execute)).getOrElse(Success()))

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
  verify: Option[T => Result]        = None):
  def offsetIs(offset: Int) = copy(asCode = markdownCode(offset = offset))
  def eval = copy(evalCode = true)
  def check[R : AsResult](f: T => R) = copy(verify = Some((t: T) => AsResult(f(t))))
/**
 * Implementation of a function to cut pieces of code by using some comments as markers
 */
case class ScissorsCutter(cutMarker: String       = scissorsMarker,
                          cutMarkerFormat: String = scissorsMarkerFormat) extends (String => String):
  def apply(text: String) =
    val split = text.split(cutMarkerFormat)

    split.zipWithIndex.
      collect { case (s, i) if i % 2 == 0 => s.removeStart("\n").removeEnd("\n") }.
      filter(_.trim.nonEmpty).mkString("\n")

object Snippet:

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

    val expression = call.removeStart("snippet")
    val trimmed =
      if s"$ls*\\{$ls*.*".r.findPrefixOf(expression).isDefined then
        expression.removeFirst(s"\\{").removeLast(s"\\}")
      else
        expression

    trimmed.removeAll("/**/;1/**/").trim
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
    if original.startsWith("\n") then    "\n\n"+multilineQuotes(cut.removeStart("\n").offset(offset))
    else if original.contains("\n") then "\n\n"+multilineQuotes(cut.offset(offset))+"\n"
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
