package org.specs2.control.eff.syntax

import org.specs2.control.eff._

object disjunction extends disjunction

trait disjunction {

  implicit class DisjunctionEffectOps[R, A](e: Eff[R, A]) {

    def runDisjunction[E, U](implicit m: Member.Aux[(E Either ?), R, U]): Eff[U, E Either A] =
      DisjunctionInterpretation.runDisjunction(e)(m)

    def runEither[E, U](implicit m: Member.Aux[(E Either ?), R, U]): Eff[U, E Either A] =
      DisjunctionInterpretation.runEither(e)(m)

    def catchLeft[E](handle: E => Eff[R, A])(implicit member: Member[(E Either ?), R]): Eff[R, A] =
      DisjunctionInterpretation.catchLeft(e)(handle)(member)

    def runLocalDisjunction[U, C, B](getter: C => B)(implicit sr: Member.Aux[C Either ?, R, U], br: (B Either ?) |= U): Eff[U, A] =
      DisjunctionInterpretation.runLocalDisjunction[R, U, C, B, A](e, getter)

  }

}
