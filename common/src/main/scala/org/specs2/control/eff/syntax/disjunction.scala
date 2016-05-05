package org.specs2.control.eff.syntax

import scalaz.\/
import org.specs2.control.eff._

object disjunction extends disjunction

trait disjunction {

  implicit class DisjunctionEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runDisjunction[E, U <: Effects](implicit member: Member.Aux[(E \/ ?), R, U]): Eff[U, E \/ A] =
      DisjunctionInterpretation.runDisjunction(e)

    def runEither[E, U <: Effects](implicit member: Member.Aux[(E \/ ?), R, U]): Eff[U, E Either A] =
      DisjunctionInterpretation.runEither(e)

    def catchLeft[E](handle: E => Eff[R, A])(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
      DisjunctionInterpretation.catchLeft(e)(handle)(member)
  }

}
