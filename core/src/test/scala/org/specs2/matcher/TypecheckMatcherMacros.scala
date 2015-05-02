package org.specs2.matcher

import org.specs2.reflect.MacroContext.Context

// these macros are used in the tests project to test typecheck matchers
object TypecheckMatcherMacros {
  def produceIncorrectCode: Unit = macro produceIncorrectCodeImpl
  def produceIncorrectCodeImpl(c: Context): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"4 ++ 5")
  }
  def produceCorrectCode: Unit = macro produceCorrectCodeImpl
  def produceCorrectCodeImpl(c: Context): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"()")
  }
  def abortMacroExpansion: Unit = macro abortMacroExpansionImpl
  def abortMacroExpansionImpl(c: Context): c.Expr[Unit] = {
    c.abort(c.enclosingPosition, "Doomed to fail!")
  }
}

