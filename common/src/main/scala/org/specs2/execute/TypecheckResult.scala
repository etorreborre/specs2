package org.specs2
package execute

import scala.compiletime.testing.*

sealed trait TypecheckResult derives CanEqual

object TypecheckSuccess extends TypecheckResult:
  override def toString: String =
    "TypecheckSuccess"

case class TypecheckErrors(errors: List[Error]) extends TypecheckResult

object TypecheckResult:

  given TypecheckResultAsResult: AsResult[TypecheckResult] with
    def asResult(t: =>TypecheckResult): Result =
      t match {
        case TypecheckErrors(errors) =>
          Failure(failureMessage(errors(0)), details = FailureDetailsMessages(errors.drop(1).map(failureMessage)))
        case TypecheckSuccess =>
          Success()
      }

  def failureMessage(e: Error): String =
    e.lineContent+"\n"+
    " "*(e.column-1) + "^" + "\n" +
    e.message
