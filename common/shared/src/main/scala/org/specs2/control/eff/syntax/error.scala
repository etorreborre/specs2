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
    def toErrorSimpleMessage: Option[String] = c.left.toOption.map(_.simpleMessage)

    def toErrorFullMessage: Option[String] = c.left.toOption.map(_.fullMessage)
  }

  implicit class ErrorOps[A](e: Error) {
    def simpleMessage: String = e.left.map(render).merge

    def fullMessage: String = e.left.map(renderWithStack).merge
  }
}
