package org.specs2.specification.create

import scala.quoted._
import org.specs2.specification.core._
import org.specs2.text.{Trim, Interpolated}
import Trim._
import org.specs2.text.NotNullStrings._
import org.specs2.control._

object S2Macro {

  def s2Implementation(sc: Expr[StringContext])(
        variables: Expr[Seq[InterpolatedFragment]],
        ff: Expr[FragmentFactory],
        postProcess: Expr[Fragments => Fragments])(using qctx: QuoteContext) : Expr[Fragments] = {
          
    val args = variables match {
      case Varargs(args) => args
      case _ => qctx.throwError("Expected statically known argument list", variables)
    }

    '{s2(${sc}.parts, ${variables}, ${ff}, ${postProcess})}

  }

   /**
   * based on the interpolated variables and the expressions captured with the macro, create the appropriate fragments
   *
   * if the Yrangepos scalac option is not set then we use an approximated method to find the expressions texts
   */
  def s2(texts: Seq[String], variables: Seq[InterpolatedFragment], ff: FragmentFactory, postProcess: Fragments => Fragments): Fragments =  {

    val location = SimpleLocation(TraceLocation("path", "fileName", "className", "methodName", lineNumber = 0))
    val fragments = (texts zip variables).foldLeft(Fragments()) { (res, cur) =>
      val (text, variable) = cur
      variable.append(res, text, location, location, "expression")
    }

    // The last piece of text is trimmed to allow the placement of closing quotes in the s2 string
    // to be on column 0 or aligned with examples and still have the same display when using the Text printer
    val last = texts.lastOption.map(_.trimEnd).filterNot(_.isEmpty).map(ff.text).toSeq

    postProcess(fragments append Fragments(last:_*))
  }


}
