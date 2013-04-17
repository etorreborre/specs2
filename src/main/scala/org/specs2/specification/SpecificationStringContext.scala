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
    def appendTo(text: String, expression: String = "") = {
      val s1 = tryOr(s)(e => s"[${e.getMessage}]")
      Fragments.createList(Text(text+s1))
    }
  }

  implicit def exampleIsSpecPart(e: Example): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = text ^ e
  }

  implicit def markdownLinkIsSpecPart(link: MarkdownLink): SpecPart = stringIsSpecPart(link.toString)

  implicit def asResultIsSpecPart[R : AsResult](r: =>R): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = {
      val texts = text.split("\n")
      val spaces = texts.last.takeWhile(Seq(' ', '\n').contains)
      val indent = spaces.mkString

      val first = texts.dropRight(1).mkString("", "\n", "\n")
      val autoExample = texts.last.trim.isEmpty

      val description = if (autoExample) CodeMarkup(expression.trim) else NoMarkup(texts.last.trim)
      val before = first + indent

      implicitly[AsResult[R]] match {
        case v : AnyValueAsResult[_] => AsResult(r) match {
          case DecoratedResult(t, e: Error) => before ^ exampleFactory.newExample(description, e)
          case DecoratedResult(t, _)        => textStart(text + t.notNull)
        }
        case other                        => before ^ exampleFactory.newExample(description, AsResult(r))
      }
    }
  }
  implicit def anyAsResultIsSpecPart(r: =>Function0Result): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = asResultIsSpecPart(AsResult(r)).appendTo(text, expression)
  }
  implicit def formIsSpecPart(f: =>Form): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = text ^ Fragments.createList(Forms.formsAreExamples(f.executeForm))
  }
  implicit def toFormIsSpecPart(f: { def form: Form}): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = formIsSpecPart(f.form).appendTo(text, expression)
  }
  implicit def fragmentIsSpecPart(f: Fragment): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = text ^ f
  }
  implicit def fragmentsIsSpecPart(fs: Fragments): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = text ^ fs
  }
  implicit def argumentsIsSpecPart(a: Arguments): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") = text ^ a
  }
  implicit class specificationInStringContext(sc: StringContext) {
    def s2(variables: SpecPart*) = macro S2Macro.s2Implementation
  }

  def s2(texts: Seq[String], variables: Seq[SpecPart], expressions: Seq[String]) = {
    val fragments = (texts zip variables zip expressions).foldLeft(Fragments() ^ interpolatedArguments) { (res, cur) =>
      val ((text, variable), expression) = cur

      // always provide the latest full piece of text to the spec part for the append method
      val (res1, text1) = res.middle.lastOption.collect { case t @ Text(_) =>
        (res.middleDropRight(1), t.t+text)
      }.getOrElse((res, text))
      res1 append variable.appendTo(text1, expression)
    }
    texts.lastOption.map(t => fragments.append(Fragments.createList(Text(t)))).getOrElse(fragments)
  }

  def interpolatedArguments = args.report(noindent = true, flow = true)
}

object S2Macro {
  def s2Implementation(c: MContext)(variables: c.Expr[SpecPart]*) : c.Expr[Fragments] = {
    import c.{universe => u}; import u.{ Position => _, _ }

    val texts = c.prefix.tree match { case Apply(_, List(Apply(_, ts))) => ts }

    val result =
      c.Expr(methodCall(c)("s2",
        toAST[List[_]](c)(texts:_*),
        toAST[List[_]](c)(variables.map(_.tree):_*),
        toAST[List[_]](c)(variables.map(stringExpr(c)(_)):_*)))

    c.Expr(atPos(c.prefix.tree.pos)(result.tree))

  }

}

trait SpecPart {
  def appendTo(text: String, expression: String = ""): Fragments
}

