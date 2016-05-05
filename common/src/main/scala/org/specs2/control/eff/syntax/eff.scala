package org.specs2.control.eff
package syntax

import scalaz.Monad
import scalaz.NaturalTransformation
import org.specs2.control.eff.Effects.|:

/**
 * Operations of Eff[R, A] values
 */
object eff extends eff

trait eff {

  implicit class EffOps[R <: Effects, A](e: Eff[R, A]) {
    def into[U](implicit f: IntoPoly[R, U, A]): Eff[U, A] =
      Eff.effInto(e)(f)

    def transform[BR, U, M[_], N[_]](t: NaturalTransformation[M, N])(implicit m: Member.Aux[M, R, U], n: Member.Aux[N, BR, U]): Eff[BR, A] =
      Interpret.transform(e, t)(m, n)
  }

  implicit class EffNoEffectOps[A](e: Eff[NoEffect, A]) {
    def run: A =
      Eff.run(e)
  }

  implicit class EffOneEffectOps[M[_] : Monad, A](e: Eff[M |: NoEffect, A]) {
    def detach: M[A] =
      Eff.detach(e)
  }

  implicit class EffMonadicOps[R <: Effects, M[_], A](e: Eff[R, M[A]]) {
    def collapse(implicit m: M <= R): Eff[R, A] =
      Eff.collapse[R, M, A](e)
  }
}
