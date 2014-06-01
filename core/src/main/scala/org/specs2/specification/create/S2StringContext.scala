package org.specs2
package specification
package create

import execute._
import text.Interpolated
import reflect.Compat210.blackbox
import reflect.Macros._
import text.NotNullStrings._
import text.Trim._
import main.Arguments
import specification.core._
import execute.DecoratedResult
import specification.core.RawText
import specification.core.Code

/**
 * Allow to use fragments inside interpolated strings starting with s2 in order to build the specification content
 */
trait S2StringContext extends FragmentsFactory { outer =>
  private val ff = fragmentFactory
  import ff._

  implicit def stringIsInterpolatedPart(s: =>String): InterpolatedPart = new InterpolatedPart {
    def append(fs: Vector[Fragment], text: String, expression: String = "") =  {
      val s1 =
        try s
        catch { case e: Throwable => s"[${e.getMessage.notNull}]" }
      fs :+ Text(text + s1)
    }
  }

  implicit def fragmentIsInterpolatedPart(f: Fragment): InterpolatedPart = new InterpolatedPart {
    def append(fs: Vector[Fragment], text: String, expression: String = "") =  {
      f match {
        // in the case of a tag which applies to the example just before,
        // if the tag is just separated by some empty text, append the tag close to the example
        case tag @ Fragment(Marker(_, _, false), _, _) if text.trim.isEmpty => fs :+ tag :+ Text(text)
        case other => fs :+ Text(text) :+ other
      }
    }
  }

  implicit def specificationLinkIsInterpolatedPart(link: SpecificationLink): InterpolatedPart =
    fragmentIsInterpolatedPart(fragmentFactory.Link(link))

  implicit def asResultIsInterpolatedPart[R : AsResult](r: =>R): InterpolatedPart = new InterpolatedPart {

    def append(fs: Vector[Fragment], text: String, expression: String = "") =  {
      val texts = text.split("\n")
      val spaces = texts.lastOption.map(_.takeWhile(Seq(' ', '\n').contains)).getOrElse("")
      val indent = spaces.mkString

      val first = if (texts.size > 1) texts.dropRight(1).mkString("", "\n", "\n") else ""
      val autoExample = texts.lastOption.exists(_.trim.isEmpty)

      val description =
        if (autoExample) Code(expression)
        else             RawText(texts.lastOption.map(_.trim).getOrElse(""))

      val before = if (first.nonEmpty) Vector(Text(first + indent)) else Vector()

      val result =
        implicitly[AsResult[R]] match {
          case v : AnyValueAsResult[_] => AsResult(r) match {
            case DecoratedResult(t, e: Error) => before :+ Example(description, e)
            case DecoratedResult(t, _)        => Vector(Text(text), Text(t.notNull))
          }
          case other                          => before :+ Example(description, AsResult(r))
        }
      fs ++ result
    }
  }


  implicit def anyAsResultIsInterpolatedPart(r: =>Function0Result): InterpolatedPart = new InterpolatedPart {
    def append(fs: Vector[Fragment], text: String, expression: String = "") = 
      asResultIsInterpolatedPart(AsResult(r)).append(fs, text, expression)
  }

  implicit def fragmentsIsInterpolatedPart(fragments: Fragments): InterpolatedPart = new InterpolatedPart {
    def append(fs: Vector[Fragment], text: String, expression: String = "") =
      (fs :+ Text(text)) ++ fragments.fragments
  }

  implicit def specStructureIsInterpolatedPart(s: SpecificationStructure): InterpolatedPart = new InterpolatedPart {
    def append(fs: Vector[Fragment], text: String, expression: String = "") = 
      (fs :+ Text(text)) ++ s.is.fragments.fragments
  }

  /**
   * based on the interpolated variables and the expressions captured with the macro, create the appropriate fragments
   *
   * if the Yrangepos scalac option is not set then we use an approximated method to find the expressions texts
   */
  def s2(content: String, Yrangepos: Boolean, texts: Seq[String], variables: Seq[InterpolatedPart], rangeExpressions: Seq[String]): SpecStructure =  {
    val expressions = if (Yrangepos) rangeExpressions else new Interpolated(content, texts).expressions

    val fragments = (texts zip variables zip expressions).foldLeft(Vector[Fragment]()) { (res, cur) =>
      val ((text, variable), expression) = cur

      // always provide the latest full piece of text to the spec part for the append method
      val (res1, text1) = res.lastOption.collect { case f @ Fragment(RawText(t), _, _) if !f.isRunnable =>
        (res.dropRight(1), t + text)
      }.getOrElse((res, text))
      variable.append(res1, text1, expression)
    }

    // The last piece of text is trimmed to allow the placement of closing quotes in the s2 string
    // to be on column 0 or aligned with examples and still have the same display when using the Text printer
    val last = texts.lastOption.map(_.trimEnd).filterNot(_.isEmpty).map(Text).toSeq

    SpecStructure(SpecHeader(outer.getClass), Arguments(), Fragments(fragments ++ last:_*))
  }

  implicit class specificationInStringContext(sc: StringContext) {
    def s2(variables: InterpolatedPart*) = macro S2Macro.s2Implementation
  }

}

object S2StringContext extends DefaultFragmentFactory

object S2Macro {
  def s2Implementation(c: blackbox.Context)(variables: c.Expr[InterpolatedPart]*) : c.Expr[SpecStructure] = {
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

trait InterpolatedPart {
  def append(parts: Vector[Fragment], text: String, expression: String = ""): Vector[Fragment]
}

