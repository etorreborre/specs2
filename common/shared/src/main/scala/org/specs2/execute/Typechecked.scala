package org.specs2
package execute

import scala.compiletime.testing.*

object Typechecked:

  given TypecheckedResultAsResult: AsResult[TypecheckResult] with
    def asResult(t: =>TypecheckResult): Result =
      t match {
        case TypecheckSuccess =>
          Success()
        case TypecheckErrors(errors) =>
          Failure("typecheck error: "+errors(0).message, details = FailureDetailsMessages(errors.map(_.message)))
      }

sealed trait TypecheckResult

object TypecheckSuccess extends TypecheckResult:
  override def toString: String =
    "TypecheckSuccess"

case class TypecheckErrors(errors: List[Error]) extends TypecheckResult
