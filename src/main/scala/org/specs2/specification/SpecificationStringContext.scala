package org.specs2
package specification

import form.Form
import main.{Arguments, ArgumentsArgs}
import execute._
import text.NotNullStrings._
import scala.reflect.macros.{Context => MContext}
import reflect.Macros._
import text.{CodeMarkup, NoMarkup, Interpolated}
import control.Exceptions._
import scala.xml.Elem
import html.MarkdownLink

/**
 * Allow to use fragments inside interpolated strings starting with s2 in order to build the specification content
 */
trait SpecificationStringContext { outer: FragmentsBuilder with ArgumentsArgs =>

  implicit def stringIsSpecPart(s: =>String): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = {
      val s1 = tryOr(s)(e => s"[${e.getMessage}]")
      fs append Fragments.createList(Text(text+s1))
    }
  }

  implicit def exampleIsSpecPart(e: Example): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs append { text ^ e }
  }

  implicit def markdownLinkIsSpecPart(link: MarkdownLink): SpecPart = stringIsSpecPart(link.toString)

  implicit def asResultIsSpecPart[R : AsResult](r: =>R): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = {
      val texts = text.split("\n")
      val spaces = texts.last.takeWhile(Seq(' ', '\n').contains)
      val indent = spaces.mkString

      val first = texts.dropRight(1).mkString("", "\n", "\n")
      val autoExample = texts.last.trim.isEmpty

      val description = if (autoExample) CodeMarkup(expression.trim) else NoMarkup(texts.last.trim)
      val before = first + indent

      val result =
        implicitly[AsResult[R]] match {
          case v : AnyValueAsResult[_] => AsResult(r) match {
            case DecoratedResult(t, e: Error) => before ^ exampleFactory.newExample(description, e)
            case DecoratedResult(t, _)        => textStart(text + t.notNull)
          }
          case other                        => before ^ exampleFactory.newExample(description, AsResult(r))
        }
      fs append result
    }
  }
  implicit def anyAsResultIsSpecPart(r: =>Function0Result): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = asResultIsSpecPart(AsResult(r)).append(fs, text, expression)
  }
  implicit def formIsSpecPart(f: =>Form): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs append { text ^ Fragments.createList(Forms.formsAreExamples(f.executeForm)) }
  }
  implicit def toFormIsSpecPart(f: { def form: Form}): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs append { formIsSpecPart(f.form).append(text, expression) }
  }
  implicit def fragmentIsSpecPart(f: Fragment): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs append { text ^ f }
  }
  implicit def fragmentsIsSpecPart(fs: Fragments): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs append { text ^ fs }
  }
  implicit def fragmentsFragmentIsSpecPart(fs: FragmentsFragment): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs append { text ^ fs }
  }
  implicit def argumentsIsSpecPart(a: Arguments): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs append { text ^ a }
  }
  implicit def specStructureIsSpecPart(s: SpecificationStructure): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs append { text ^ s.content }
  }

  implicit class specificationInStringContext(sc: StringContext) {
    def s2(variables: SpecPart*) = macro S2Macro.s2Implementation
  }

  /**
   * based on the interpolated variables and the expressions captured with the macro, create the appropriate fragments
   *
   * if the Yrangepos scalac option is not set then we use an approximated method to find the expressions texts
   */
  def s2(content: String, Yrangepos: Boolean, texts: Seq[String], variables: Seq[SpecPart], rangeExpressions: Seq[String]) = {
    val expressions = if (Yrangepos) rangeExpressions else new Interpolated(content, texts).expressions

    val fragments = (texts zip variables zip expressions).foldLeft(Fragments() ^ interpolatedArguments) { (res, cur) =>
      val ((text, variable), expression) = cur

      // always provide the latest full piece of text to the spec part for the append method
      val (res1, text1) = res.middle.lastOption.collect { case t @ Text(_) =>
        (res.middleDropRight(1), t.t+text)
      }.getOrElse((res, text))
      variable.append(res1, text1, expression)
    }
    texts.lastOption.map(t => fragments.append(Fragments.createList(Text(t)))).getOrElse(fragments)
  }

  def interpolatedArguments = args.report(noindent = true, flow = true)
}

object S2Macro {
  def s2Implementation(c: MContext)(variables: c.Expr[SpecPart]*) : c.Expr[Fragments] = {
    import c.{universe => u}; import u.{ Position => _, _ }

    val texts = c.prefix.tree match { case Apply(_, List(Apply(_, ts))) => ts }

    val macroPos = c.macroApplication.pos
    val fileContent = macroPos.source.content.mkString

    def contentFrom(pos: c.Position) = fileContent.split("\n").drop(pos.line - 1).mkString("\n").drop(pos.column-1)
    val content = contentFrom(macroPos).drop("s2\"\"\"".size)
    val Yrangepos = macroPos.isRange

    val result =
      c.Expr(methodCall(c)("s2",
        c.literal(content).tree,
        c.literal(Yrangepos).tree,
        toAST[List[_]](c)(texts:_*),
        toAST[List[_]](c)(variables.map(_.tree):_*),
        toAST[List[_]](c)(variables.map(stringExpr(c)(_)):_*)))

    c.Expr(atPos(c.prefix.tree.pos)(result.tree))

  }

}

trait SpecPart {
  def append(fs: Fragments, text: String, expression: String = ""): Fragments
}

