package org.specs2
package execute


/**
 * result of the typechecking of some code
 */
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
