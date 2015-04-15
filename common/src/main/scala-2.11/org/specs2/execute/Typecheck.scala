package org.specs2
package execute

import reflect.Macros
import reflect.MacroContext._
import Macros._
import scala.reflect.macros.{ParseException, TypecheckException}

/**
 * This macro checks if some code can be parsed and typechecks ok
 *
 * Credits:
 *
 *  - Stefan Zeiger (@StefanZeiger) for the typecheck method
 *  - Jean-Remi Desjardins (@jrdesjardins) for the tc interpolator
 */
object Typecheck {

  /**
   * Typecheck code and fail at runtime if the code doesn't typecheck
   * If the code doesn't parse there will be a compile-time error
   */
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
        }

      case other => q"""Typechecked("", CanTypecheckLiteralsOnly)"""
    }
  }

  /**
   * Typecheck code and fail at runtime if the code doesn't parse or typecheck
   */
  def parseAndTypecheck(code: String): Typechecked = macro parseAndTypecheckImpl

  def parseAndTypecheckImpl(c: Context)(code: c.Expr[String]): c.Tree = {
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

  /**
   * Typecheck code and fail at runtime if the code doesn't typecheck
   * If the code doesn't parse there will be a compile-time error
   *
   * @return the parsed/typechecked code if it is ok or a Typechecked object otherwise
   */
  implicit class typecheckQuote(val sc: StringContext) extends AnyVal {
    def tc(variables: Any*): Any = macro typecheckCode
  }

  def typecheckCode(c: Context)(variables: c.Expr[Any]*) : c.Tree = {
    import c.{universe => u}; import u.{ Position => _, _ }

    val texts = c.prefix.tree match { case Apply(_, List(Apply(_, ts))) => ts }
    if (texts.size != 1)
      q"""Typechecked(${texts.mkString}, TypecheckError("can only typecheck an interpolated string with no variables at the moment"))"""
    else {
      val code = texts.head.asInstanceOf[Literal].value.value.asInstanceOf[String]
      try c.typecheck(c.parse(code))
      catch {
        case TypecheckException(_, m) => q"Typechecked($code, TypecheckError($m))"
      }
    }
  }

  /**
   * Typecheck code and fail at runtime if the code doesn't parse or typecheck
   *
   * @return the parsed/typechecked code if it is ok or a Typechecked object otherwise
   */
  implicit class parseAndTypecheckQuote(val sc: StringContext) extends AnyVal {
    def ptc(variables: Any*): Any = macro parseAndTypecheckCode
  }

  def parseAndTypecheckCode(c: Context)(variables: c.Expr[Any]*) : c.Tree = {
    import c.{universe => u}; import u.{ Position => _, _ }

    val texts = c.prefix.tree match { case Apply(_, List(Apply(_, ts))) => ts }
    if (texts.size != 1)
      q"""Typechecked(${texts.mkString}, TypecheckError("can only typecheck an interpolated string with no variables at the moment"))"""
    else {
      val code = texts.head.asInstanceOf[Literal].value.value.asInstanceOf[String]
      try c.typecheck(c.parse(code))
      catch {
        case TypecheckException(_, m) => q"Typechecked($code, TypecheckError($m))"
        case ParseException(_, m)     => q"Typechecked($code, ParseError($m))"
        case e: Exception             => q"Typechecked($code, UnexpectedTypecheckError(${e.getMessage}))"
      }
    }
  }
}