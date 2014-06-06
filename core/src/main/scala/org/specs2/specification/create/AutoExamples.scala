package org.specs2
package specification
package create

import execute.AsResult
import reflect.Compat210.blackbox
import reflect.Macros
import text.Trim._
import specification.core.{Code, Fragments, Fragment}

/**
 * This trait allows to capture some code as an example description
 */
trait AutoExamples extends FragmentsFactory {
  implicit def eg[T : AsResult](code: =>T): Fragments = macro AutoExamples.create[T]

  def createExample[T](expression: String, code: =>T, asResult: AsResult[T]): Fragments =
    Fragments(fragmentFactory.Example(Code(trimExpression(expression)), code)(asResult),
              fragmentFactory.Break)

  private[specs2] def trimExpression(call: String) = {
    call.
      trimEnclosing("${", "}").
      trimStart("eg").
      trimEnclosing("{", "}").
      trimEnclosing("`", "`").
      removeFirst("`\\(.*\\)").trimFirst("`").split("\n", -1).map(_.trim).mkString("\n")
  }

  private def containsAccolade(expression: String) =
    s"$ls*\\{$ls*.*".r.findPrefixOf(expression).isDefined

  private lazy val ls = "[ \t\\x0B\f]"
}

object AutoExamples extends AutoExamples {
  def create[T](c: blackbox.Context)(code: c.Expr[T])(asResult: c.Expr[AsResult[T]]): c.Expr[Fragments] = {
    import c.{universe => u}; import u._
    import Macros._
    val result = c.Expr(methodCall(c)("createExample", stringExprMacroPos(c)(code), code.tree.duplicate, asResult.tree))
    c.Expr(atPos(c.prefix.tree.pos)(result.tree))
  }
}

