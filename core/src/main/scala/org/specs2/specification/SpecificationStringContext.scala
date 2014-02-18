package org.specs2
package specification

import main.{Arguments, ArgumentsArgs}
import execute._
import text.NotNullStrings._
import reflect.Macros._
import reflect.Compat210._
import text.Interpolated
import text.NotNullStrings._
import control.Exceptions._
import specification.TagFragments._

/**
 * Allow to use fragments inside interpolated strings starting with s2 in order to build the specification content
 */
trait SpecificationStringContext { outer: FragmentsBuilder with ArgumentsArgs with FormattingTags =>

  implicit def stringIsSpecPart(s: =>String): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = {
      val s1 = tryOr(s)(e => s"[${e.getMessage.notNull}]")
      fs append createTextFragment(text+s1).fragments
    }
  }

  implicit def exampleIsSpecPart(e: Example): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs.append(createTextFragment(text)).append(e)
  }

  implicit def asResultIsSpecPart[R : AsResult](r: =>R): SpecPart = new SpecPart {

    def append(fs: Fragments, text: String, expression: String = "") = {
      val texts = text.split("\n")
      val spaces = texts.lastOption.map(_.takeWhile(Seq(' ', '\n').contains)).getOrElse("")
      val indent = spaces.mkString

      val first = texts.dropRight(1).mkString("", "\n", "\n")
      val autoExample = texts.lastOption.exists(_.trim.isEmpty)

      val description =
        if (autoExample) FormattedString.code(expression).withFlow
        else             FormattedString(texts.lastOption.map(_.trim).getOrElse("")).withFlow

      val before = first + indent

      val result =
        implicitly[AsResult[R]] match {
          case v : AnyValueAsResult[_] => AsResult(r) match {
            case DecoratedResult(t, e: Error) => createTextFragment(before).append(exampleFactory.newExample(description, e))
            case DecoratedResult(t, _)        => createTextFragment(text).append(createTextFragment(t.notNull).fragments)
          }
          case other                        => createTextFragment(before).append(exampleFactory.newExample(description, AsResult(r)))
        }
      fs append result.middle
    }
  }
  implicit def anyAsResultIsSpecPart(r: =>Function0Result): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = asResultIsSpecPart(AsResult(r)).append(fs, text, expression)
  }
  implicit def fragmentIsSpecPart(f: Fragment): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = f match {
      // in the case of a tag which applies to the example just before,
      // if the tag is just separated by some empty text, append the tag close to the example
      case t: TaggedAs if text.trim.isEmpty => fs.append(t).append(createTextFragment(text))
      case other                            => fs.append(createTextFragment(text)).add(other)
    }
  }
  implicit def fragmentsIsSpecPart(fragments: Fragments): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs.append(createTextFragment(text)).append(fragments)
  }
  implicit def fragmentsFragmentIsSpecPart(ffs: FragmentsFragment): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs.append(createTextFragment(text)).append(ffs.fragments)
  }
  implicit def argumentsIsSpecPart(a: Arguments): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs.append(createTextFragment(text)).add(a)
  }
  implicit def specStructureIsSpecPart(s: SpecificationStructure): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs.append(createTextFragment(text)).append(s.content)
  }
  implicit def markdownLinkIsSpecPart(link: MarkdownLink): SpecPart = stringIsSpecPart(link.toString)

  implicit class specificationInStringContext(sc: StringContext) {
    def s2(variables: SpecPart*): Fragments = macro S2Macro.s2Implementation
  }

  /**
   * based on the interpolated variables and the expressions captured with the macro, create the appropriate fragments
   *
   * if the Yrangepos scalac option is not set then we use an approximated method to find the expressions texts
   */
  def s2(content: String, Yrangepos: Boolean, texts: Seq[String], variables: Seq[SpecPart], rangeExpressions: Seq[String]) = {
    val expressions = if (Yrangepos) rangeExpressions else new Interpolated(content, texts).expressions

    val fragments = (texts zip variables zip expressions).foldLeft(Fragments.createList()) { (res, cur) =>
      val ((text, variable), expression) = cur

      // always provide the latest full piece of text to the spec part for the append method
      val (res1, text1) = res.middle.lastOption.collect { case t: Text =>
        (res.middleDropRight(1), t.t+text)
      }.getOrElse((res, text))
      variable.append(res1, text1, expression)
    }
    formatSection(flow = true, markdown = true) ^ texts.lastOption.map(t => fragments append createTextFragment(t).fragments).getOrElse(fragments) ^ formatSection(flow = true, markdown = true)
  }

  private[specs2] def createTextFragment(s: String): FragmentsFragment = fragments(Text.create(FormattedString(s)))
}

object S2Macro {
  import scala.reflect.macros._
  def s2Implementation(c: blackbox.Context)(variables: c.Expr[SpecPart]*) : c.Expr[Fragments] = {
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

