package org.specs2.matcher.describe.constraints

import scala.language.experimental.macros
import scala.reflect.macros.blackbox


final class IsCaseClass[T]

object IsCaseClass {
  def checkCaseClassMacro[T : ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[IsCaseClass[T]] = {
    val T = ctx.weakTypeOf[T]
    if (!T.typeSymbol.isClass || !T.typeSymbol.asClass.isCaseClass)
      ctx.error(ctx.enclosingPosition,s"$T does not have case modifier")

    import ctx.universe._
    ctx.Expr[IsCaseClass[T]](q"new _root_.org.specs2.matcher.describe.constraints.IsCaseClass[$T]")
  }

  implicit def checkCaseClass[T]: IsCaseClass[T] = macro checkCaseClassMacro[T]
}
