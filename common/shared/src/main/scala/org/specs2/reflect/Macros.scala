package org.specs2.reflect

import org.specs2.reflect.MacroContext._

/**
 * Macros helper functions
 */
object Macros {

  def toAST[A](c: Context)(xs: c.Tree*)(implicit tt: c.TypeTag[A]): c.Tree = {
    import c.universe._
    Apply(Select(Ident(typeOf[A].typeSymbol.companion), TermName("apply")), xs.toList)
  }

  def methodCall(c: Context)(name: String, xs: c.Tree*): c.Tree = {
    import c.universe._
    Apply(Ident(TermName(name)), xs.toList)
  }

  def stringExprMacroPos(c: Context)(variable: c.Expr[Any]): c.Tree = {
    import c.universe._
    q"${sourceOf(c)(variable)(c.macroApplication.pos)}"
  }

  def stringExpr(c: Context)(variable: c.Expr[Any]): c.Tree = {
    import c.universe._
    q"${sourceOf(c)(variable)(variable.tree.pos)}"
  }

  def sourceOf(c: Context)(expr: c.Expr[_])(p: c.Position): String = {
    val source = new String(p.source.content)
    if (p.isRange) source.substring(p.start, p.end)
    else p.source.lineToString(p.line - 1).toString.substring(p.point - p.source.lineToOffset(p.source.offsetToLine(p.point)))
  }

  def termName(c: Context)(m: c.Expr[Any]): c.Expr[String] = {
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
    c.Expr(q"${name.toString.trim}")
  }

}
