package org.specs2.control
package eff

import Eff._
import disjunction._
import syntax.disjunction._

import scalaz.{-\/, \/, \/-, ~>}

/**
 * This effect is used in the implementation of the Async effect
 */
object SubscribeEffect {

  type Callback[A] = (Throwable \/ A) => Unit

  type Subscribe[A] = Callback[A] => Unit
  type AttemptedSubscribe[A] = Callback[Throwable \/ A] => Unit

  type _subscribe[R] = Subscribe |= R

  type FS = Fx.fx1[Subscribe]

  def subscribeToAttemptedSubscribe = new (Subscribe ~> AttemptedSubscribe) {
    def apply[X](subscribe: Subscribe[X]): AttemptedSubscribe[X] =
      (c: Callback[Throwable \/ X]) => subscribe((tx: Throwable \/ X) => c(\/-(tx)))
  }

  def subscribeAttempt[A](e: Eff[FS, A])(implicit m: Subscribe /= FS): Eff[FS, ThrowableOr[A]] = {
    type U = Fx.prepend[ThrowableOr, FS]

    interpret.translateInto[FS, Subscribe, U, A](e)(new Translate[Subscribe, U] {
      def apply[X](sx: Subscribe[X]): Eff[U, X] =
        send[Subscribe, U, ThrowableOr[X]]((c: Callback[Throwable \/ X]) => sx.apply((tx: Throwable \/ X) => c(\/-(tx)))).flatMap {
          case -\/(t)  => left[U, Throwable, X](t)
          case \/-(x) => right[U, Throwable, X](x)
        }
    }).runDisjunction
  }

}


