package org.specs2
package reflect

import scala.reflect.macros.{Context => MContext}

object Macros {

  def toAST[A](c: MContext)(xs: c.Tree*)(implicit tt: c.TypeTag[A]): c.Tree = {
    import c.universe._
    Apply(Select(Ident(typeOf[A].typeSymbol.companionSymbol), newTermName("apply")), xs.toList)
  }

  def methodCall(c: MContext)(name: String, xs: c.Tree*): c.Tree = {
    import c.universe._
    Apply(Select(This(tpnme.EMPTY), newTermName(name)), xs.toList)
  }

  def stringExpr(c: MContext)(variable: c.Expr[Any]): c.Tree =
    c.literal(sourceOf(c)(variable)).tree

  def sourceOf(c: MContext)(expr: c.Expr[_]): String = {
    val p = expr.tree.pos
    val source = new String(p.source.content)
    if (p.isRange) source.substring(p.start, p.end)
    else p.lineContent.substring(p.point - p.source.lineToOffset(p.source.offsetToLine(p.point)))
  }


}
