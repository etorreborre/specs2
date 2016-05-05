package org.specs2.control.eff.syntax

import org.specs2.control.eff._

object warnings extends warnings

trait warnings {

  implicit class WarningsEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runWarnings[U <: Effects](implicit member: Member.Aux[Warnings, R, U]): Eff[U, (A, List[String])] =
      WarningsEffect.runWarnings(e)

  }

}
