package org.specs2.execute

import scala.quoted.*
import scala.compiletime.testing.*

/**
 * This macro checks if some code can be parsed and typechecks ok
 */
object Typecheck:

  /**
   * Typecheck code and fail at runtime if the code doesn't typecheck
   * If the code doesn't parse there will be a compile-time error
   */
  transparent inline def apply(code: String): TypecheckResult =
    ${typecheckCode('{typeCheckErrors(code)})}

  /** alias for apply */
  transparent inline def typecheck(code: String): TypecheckResult =
    ${typecheckCode('{typeCheckErrors(code)})}

  def typecheckCode(typeCheckErrors: Expr[List[Error]])(using qctx: Quotes): Expr[TypecheckResult] =
    import qctx.reflect.*
    '{ if ${typeCheckErrors}.isEmpty
         then org.specs2.execute.TypecheckSuccess
         else org.specs2.execute.TypecheckErrors(${typeCheckErrors})

    }
