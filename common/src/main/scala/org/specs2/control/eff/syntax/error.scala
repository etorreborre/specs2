package org.specs2.control.eff
package syntax

import ErrorEffect._
import scala.reflect.ClassTag

object error extends error

trait error {

  implicit class ErrorEffectOps[R, A](action: Eff[R, A]) {

    def runError(implicit m: Member[ErrorOrOk, R]): Eff[m.Out, Error Either A] =
      ErrorEffect.runError(action)(m.aux)

    def andFinally(last: Eff[R, Unit])(implicit m: ErrorOrOk /= R): Eff[R, A] =
      ErrorEffect.andFinally(action, last)

    def orElse(action2: Eff[R, A])(implicit m: ErrorOrOk /= R): Eff[R, A] =
      ErrorEffect.orElse(action, action2)

    def ignore[E <: Throwable : ClassTag](implicit m: ErrorOrOk /= R): Eff[R, Unit] =
      ErrorEffect.ignoreException(action)
  }

  implicit class ErrorOrOkOps[A](c: Error Either A) {
    def toErrorSimpleMessage: Option[String] =
      c match {
        case Left(e) => Some(e.simpleMessage)
        case _      => None
      }

    def toErrorFullMessage: Option[String] =
      c match {
        case Left(e) => Some(e.fullMessage)
        case _      => None
      }
  }

  implicit class ErrorOps[A](e: Error) {
    def simpleMessage: String =
      e match {
        case Left(t) => render(t)
        case Right(m) => m
      }

    def fullMessage: String =
      e match {
        case Left(t) => renderWithStack(t)
        case Right(m) => m
      }
  }


}
