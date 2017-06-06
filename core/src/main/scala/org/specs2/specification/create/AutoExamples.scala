package org.specs2
package specification
package create

import execute.AsResult
import reflect.MacroContext._
import reflect.Macros
import text.Trim._
import org.specs2.specification.core.{Description, Fragments}

/**
 * This trait allows to create examples where the description is the code itself
 * and the code returns an AsResult value
 */
trait AutoExamples extends FragmentsFactory {

  implicit def eg[T : AsResult](code: =>T): Fragments = macro AutoExamples.create[T]

  def createExample[T](expression: String, code: =>T, asResult: AsResult[T]): Fragments =
    Fragments(fragmentFactory.example(Description.code(trimExpression(expression)), code)(asResult))

  private[specs2] def trimExpression(call: String) = {
    call.
      trimEnclosing("${", "}").
      trimStart("eg").
      trimEnclosing("{", "}").
      trimEnclosing("`", "`").
      removeFirst("`\\(.*\\)").trimFirst("`").split("\n", -1).map(_.trim).mkString("\n")
  }

}

object AutoExamples extends AutoExamples {

  def create[T](c: Context)(code: c.Expr[T])(asResult: c.Expr[AsResult[T]]): c.Expr[Fragments] = {
    import c.{universe => u}; import u._
    import Macros._
    val result = c.Expr(methodCall(c)("createExample", stringExprMacroPos(c)(code), code.tree.duplicate, asResult.tree))
    c.Expr(atPos(c.prefix.tree.pos)(result.tree))
  }

}

