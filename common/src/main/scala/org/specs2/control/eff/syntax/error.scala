package org.specs2.control.eff
package syntax

import Member.<=
import ErrorEffect._
import scalaz._

object error extends error

trait error {

  implicit class ErrorEffectOps[R <: Effects, A](action: Eff[R, A]) {

    def runError[U <: Effects](implicit m: Member.Aux[ErrorOrOk, R, U]): Eff[U, Error \/ A] =
      ErrorEffect.runError(action)

    def andFinally(last: Eff[R, Unit])(implicit m: ErrorOrOk <= R): Eff[R, A] =
      ErrorEffect.andFinally(action, last)

    def orElse(action2: Eff[R, A])(implicit m: ErrorOrOk <= R): Eff[R, A] =
      ErrorEffect.orElse(action, action2)
  }

  implicit class ErrorOrOkOps[A](c: Error \/ A) {
    def toErrorSimpleMessage: Option[String] =
      c match {
        case -\/(e) => Some(e.simpleMessage)
        case _      => None
      }

    def toErrorFullMessage: Option[String] =
      c match {
        case -\/(e) => Some(e.fullMessage)
        case _      => None
      }
  }

  implicit class ErrorOps[A](e: Error) {
    def simpleMessage: String =
      e match {
        case -\/(t) => render(t)
        case \/-(m) => m
      }

    def fullMessage: String =
      e match {
        case -\/(t) => renderWithStack(t)
        case \/-(m) => m
      }
  }


}
