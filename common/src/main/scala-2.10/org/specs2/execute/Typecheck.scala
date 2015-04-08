package org.specs2
package execute

import reflect.MacroContext._

import scala.reflect.macros.{ParseException, TypecheckException}

/**
 * This macro checks if some code can be parsed and typechecks ok
 *
 * Credit: Stefan Zeiger (@StefanZeiger)
 */
object Typecheck {

  def apply(code: String): Typechecked = macro typecheckImpl

  /** alias for apply */
  def typecheck(code: String): Typechecked = macro typecheckImpl

  def typecheckImpl(c: Context)(code: c.Expr[String]): c.Expr[Typechecked] = {
    import c.universe._
    c.Expr {
      code match {
        case Expr(Literal(Constant(codeString: String))) =>
          try {
            c.typeCheck(c.parse(codeString))
            q"Typechecked($codeString, TypecheckSuccess)"
          } catch {
            case TypecheckException(_, m) => q"Typechecked($codeString, TypecheckError($m))"
            case ParseException(_, m)     => q"Typechecked($codeString, ParseError($m))"
            case e: Exception             => q"Typechecked($codeString, UnexpectedTypecheckError(${e.getMessage}))"
          }

        case other => q"""Typechecked("", CanTypecheckLiteralsOnly)"""
      }
    }
  }
}
