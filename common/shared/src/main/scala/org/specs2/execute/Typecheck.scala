package org.specs2.execute

import org.specs2.reflect.MacroContext.Context

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
  def apply(params: TypecheckParams)(code: String): Typechecked = macro typecheckExceptImpl

  /** alias for apply */
  def typecheck(code: String): Typechecked = macro typecheckImpl
  def typecheckImpl(c: Context)(code: c.Expr[String]): c.Tree = {
    import c.universe._
    typecheckImplementation(c)(code, c.Expr(q"org.specs2.execute.TypecheckParams()"))
  }

  def typecheckWith(params: TypecheckParams)(code: String): Typechecked = macro typecheckExceptImpl
  def typecheckExceptImpl(c: Context)(params: c.Expr[TypecheckParams])(code: c.Expr[String]): c.Tree =
    typecheckImplementation(c)(code, params)

  def typecheckImplementation(c: Context)(code: c.Expr[String], params: c.Expr[TypecheckParams]): c.Tree = {
    import c.universe._
    code match {
      case Expr(Literal(Constant(codeString: String))) =>
        // evaluate the parameters
        val ps = try {
          c.eval(c.Expr(c.untypecheck(params.tree))).asInstanceOf[TypecheckParams]
        } catch { case e: Exception => c.abort(c.enclosingPosition, "typecheck parameters must be passed directly to the typecheck macro") }

        try {
          // parse without macros first
          val parsed = try {
            c.parse(codeString)
          } catch {
            case ParseException(_, m) if ps.deferParsing =>
              q"org.specs2.execute.Typechecked($codeString, org.specs2.execute.ParseError($m))"
            case ParseException(_, m) if !ps.deferParsing =>
              c.abort(c.enclosingPosition, m)
            case e: Exception =>
              q"org.specs2.execute.Typechecked($codeString, org.specs2.execute.UnexpectedTypecheckError(${e.getMessage}))"
          }
          c.typecheck(parsed, withMacrosDisabled = true)
          // if that's ok parse with macros
          try {
            c.typecheck(parsed)
            q"org.specs2.execute.Typechecked($codeString, org.specs2.execute.TypecheckSuccess)"
          } catch {
            // got a typecheck exception with macros
            case TypecheckException(_, m) if ps.deferMacros =>
              q"org.specs2.execute.Typechecked($codeString, org.specs2.execute.TypecheckError($m))"
            case TypecheckException(_, m) if !ps.deferMacros =>
              c.abort(c.enclosingPosition, m)
            case e: Exception =>
              q"org.specs2.execute.Typechecked($codeString, org.specs2.execute.UnexpectedTypecheckError(${e.getMessage}))"
          }
        } catch {
          // got a typecheck exception without macros
          // if implicit errors are not deferred and this is an implicit error
          case TypecheckException(_, m) if !ps.deferImplicits && m.startsWith("could not find implicit value") =>
            c.abort(c.enclosingPosition, m)
          case TypecheckException(_, m) =>
            q"org.specs2.execute.Typechecked($codeString, org.specs2.execute.TypecheckError($m))"
        }

      case other => q"""org.specs2.execute.Typechecked("", CanTypecheckLiteralsOnly)"""
    }
  }

  /**
   * Typecheck code and fail at runtime if the code doesn't typecheck
   * If the code doesn't parse there will be a compile-time error
   *
   * @return the parsed/typechecked code if it is ok or a Typechecked object otherwise
   */
  implicit class typecheckQuote(val sc: StringContext) extends AnyVal {
    def tc(variables: Any*): Any = macro typecheckCodeImpl
  }

  def typecheckCodeImpl(c: Context)(variables: c.Expr[Any]*): c.Tree = {
    import c.universe._
    typecheckCodeImplementation(c)(variables:_*)(c.Expr(q"org.specs2.execute.TypecheckParams()"))
  }

  implicit class typecheckWithQuote(val sc: StringContext) extends AnyVal {
    def tcw(variables: Any*)(params: TypecheckParams): Any = macro typecheckCodeImplementation
  }

  def typecheckCodeImplementation(c: Context)(variables: c.Expr[Any]*)(params: c.Expr[TypecheckParams]) : c.Tree = {
    import c.{universe => u}
    import u.{Position => _, _}

    val texts = c.prefix.tree match { case Apply(_, List(Apply(_, ts))) => ts }
    if (texts.size != 1)
      q"""org.specs2.execute.Typechecked(${texts.mkString}, TypecheckError("can only typecheck an interpolated string with no variables at the moment"))"""
    else {
      val codeString = texts.head.asInstanceOf[Literal].value.value.asInstanceOf[String]
        // evaluate the parameters
        val ps = try {
          c.eval(c.Expr(c.untypecheck(params.tree))).asInstanceOf[TypecheckParams]
        } catch { case e: Exception => c.abort(c.enclosingPosition, "typecheck parameters must be passed directly to the typecheck macro") }
        try {
          // parse without macros first
          val parsed = try {
            c.parse(codeString)
          } catch {
            case ParseException(_, m) if ps.deferParsing =>
              q"org.specs2.execute.Typechecked($codeString, org.specs2.execute.ParseError($m))"
            case ParseException(_, m) if !ps.deferParsing =>
              c.abort(c.enclosingPosition, m)
            case e: Exception =>
              q"org.specs2.execute.Typechecked($codeString, org.specs2.execute.UnexpectedTypecheckError(${e.getMessage}))"
          }
          c.typecheck(parsed, withMacrosDisabled = true)
          // if that's ok parse with macros
          try {
            c.typecheck(parsed)
          } catch {
            // got a typecheck exception with macros
            case TypecheckException(_, m) if ps.deferMacros =>
              q"org.specs2.execute.Typechecked($codeString, org.specs2.execute.TypecheckError($m))"
            case TypecheckException(_, m) if !ps.deferMacros =>
              c.abort(c.enclosingPosition, m)
            case e: Exception =>
              q"org.specs2.execute.Typechecked($codeString, org.specs2.execute.UnexpectedTypecheckError(${e.getMessage}))"
          }
        } catch {
          // got a typecheck exception without macros
          // if implicit errors are not deferred and this is an implicit error
          case TypecheckException(_, m) if !ps.deferImplicits && m.startsWith("could not find implicit value") =>
            c.abort(c.enclosingPosition, m)
          case TypecheckException(_, m) =>
            q"org.specs2.execute.Typechecked($codeString, org.specs2.execute.TypecheckError($m))"
        }
    }
  }

  val macrosAtCompileTime    = TypecheckParams(_deferMacros = Some(false))
  val implicitsAtCompileTime = TypecheckParams(_deferImplicits = Some(false))
  val parsingAtRuntime       = TypecheckParams(_deferParsing = Some(true))
}

case class TypecheckParams(_deferMacros: Option[Boolean] = None, _deferImplicits: Option[Boolean] = None, _deferParsing: Option[Boolean] = None) {

  // by default we want macro expansion errors at runtime
  def deferMacros = _deferMacros.getOrElse(true)

  // by default we want implicit errors at runtime
  def deferImplicits = _deferImplicits.getOrElse(true)

  // by default we want parsing errors at compile time
  def deferParsing = _deferParsing.getOrElse(false)

  def <|(other: TypecheckParams): TypecheckParams =
    TypecheckParams(
      _deferMacros    = _deferMacros.orElse(other._deferMacros),
      _deferImplicits = _deferImplicits.orElse(other._deferImplicits),
      _deferParsing = _deferParsing.orElse(other._deferParsing)
    )
}
