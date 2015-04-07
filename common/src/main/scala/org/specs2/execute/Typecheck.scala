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

  def typecheckImpl(c: Context)(code: c.Expr[String]): c.Tree = {
    import c.universe._

    code match {
      case Expr(Literal(Constant(codeString: String))) =>
        try {
          c.typecheck(c.parse(codeString))
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

case class Typechecked(code: String, result: TypecheckResult) {
  def isSuccess: Boolean =
    result == TypecheckSuccess
}

object Typechecked {
  implicit def TypecheckedAsResult: AsResult[Typechecked] = new AsResult[Typechecked] {
    def asResult(t: =>Typechecked): Result = {
      t.result match {
        case TypecheckSuccess            => Success()
        case CanTypecheckLiteralsOnly    => Error("only literals can be typechecked")
        case TypecheckError(m)           => Failure("typecheck error: "+m)
        case ParseError(m)               => Failure("parse error: "+m)
        case UnexpectedTypecheckError(m) => Failure("unexpected error: "+m)
      }
    }
  }
}

sealed trait TypecheckResult

object TypecheckSuccess extends TypecheckResult
object CanTypecheckLiteralsOnly extends TypecheckResult
case class TypecheckError(message: String) extends TypecheckResult
case class ParseError(message: String) extends TypecheckResult
case class UnexpectedTypecheckError(message: String) extends TypecheckResult