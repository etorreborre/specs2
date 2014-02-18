package org.specs2
package reflect

import Compat210._

object Macros {
  import scala.reflect.macros._

  def toAST[A](c: blackbox.Context)(xs: c.Tree*)(implicit tt: c.TypeTag[A]): c.Tree = {
    import c.universe._
    Apply(Select(Ident(typeOf[A].typeSymbol.companionSymbol), newTermName("apply")), xs.toList)
  }

  def methodCall(c: blackbox.Context)(name: String, xs: c.Tree*): c.Tree = {
    import c.universe._
    Apply(Ident(newTermName(name)), xs.toList)
  }

  def stringExprMacroPos(c: blackbox.Context)(variable: c.Expr[Any]): c.Tree =
    c.literal(sourceOf(c)(variable)(c.macroApplication.pos)).tree

  def stringExpr(c: blackbox.Context)(variable: c.Expr[Any]): c.Tree =
    c.literal(sourceOf(c)(variable)(variable.tree.pos)).tree

  def sourceOf(c: blackbox.Context)(expr: c.Expr[_])(p: c.Position): String = {
    val source = new String(p.source.content)
    if (p.isRange) source.substring(p.start, p.end)
    else p.lineContent.substring(p.point - p.source.lineToOffset(p.source.offsetToLine(p.point)))
  }

  def termName(c: blackbox.Context)(m: c.Expr[Any]): c.Expr[String] = {
    import c.universe._
    val name = m.tree match {
      case Ident(termName)                                       => termName
      case Select(_, termName)                                   => termName
      case Apply(Select(_, termName), _)                         => termName
      case Apply(Ident(termName), _)                             => termName
      case Apply(TypeApply(Ident(termName), _), _)               => termName
      case Apply(TypeApply(Select(_, termName), _), _)           => termName
      case Apply(Apply(TypeApply(Ident(termName), _), _), _)     => termName
      case Apply(Apply(TypeApply(Select(_, termName), _), _), _) => termName
      case Function(_, Apply(Select(_, termName), _))            => termName
      case other                                                 => c.abort(m.tree.pos, "The code must be a member selection, or a function application:\n"+showRaw(m.tree))
    }
    c.literal(name.toString.trim)
  }

}

/**
 * to remove 2.11 warnings
 */
object Compat210 {
  object blackbox {
    type Context = scala.reflect.macros.Context
  }
}
