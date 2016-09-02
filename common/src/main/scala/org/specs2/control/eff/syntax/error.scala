package org.specs2.control.eff
package syntax

import Member.<=
import ErrorEffect._
import scalaz._
import scala.reflect.ClassTag

object error extends error

trait error {

  implicit class ErrorEffectOps[R, A](action: Eff[R, A]) {

    def runError(implicit m: Member[ErrorOrOk, R]): Eff[m.Out, Error \/ A] =
      ErrorEffect.runError(action)(m.aux)

    def andFinally(last: Eff[R, Unit])(implicit m: ErrorOrOk <= R): Eff[R, A] =
      ErrorEffect.andFinally(action, last)

    def orElse(action2: Eff[R, A])(implicit m: ErrorOrOk <= R): Eff[R, A] =
      ErrorEffect.orElse(action, action2)

    def ignore[E <: Throwable : ClassTag](implicit m: ErrorOrOk <= R): Eff[R, Unit] =
      ErrorEffect.ignoreException(action)
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
