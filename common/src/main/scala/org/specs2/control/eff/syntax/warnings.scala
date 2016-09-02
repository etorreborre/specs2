package org.specs2.control.eff
package syntax

import WarningsEffect._

object warnings extends warnings

trait warnings {

  implicit class WarningsEffectOps[R, A](action: Eff[R, A]) {

    def runWarnings[U](implicit m: Member.Aux[Warnings, R, U]): Eff[U, (A, List[String])] =
      WarningsEffect.runWarnings(action)

  }

}

