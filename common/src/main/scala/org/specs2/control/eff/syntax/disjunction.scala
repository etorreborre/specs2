package org.specs2.control.eff.syntax

import scalaz._
import org.specs2.control.eff._

object disjunction extends disjunction

trait disjunction {

  implicit class DisjunctionEffectOps[R, A](e: Eff[R, A]) {

    def runDisjunction[E, U](implicit m: Member.Aux[(E \/ ?), R, U]): Eff[U, E \/ A] =
      DisjunctionInterpretation.runDisjunction(e)(m)

    def runEither[E, U](implicit m: Member.Aux[(E \/ ?), R, U]): Eff[U, E Either A] =
      DisjunctionInterpretation.runEither(e)(m)

    def catchLeft[E](handle: E => Eff[R, A])(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
      DisjunctionInterpretation.catchLeft(e)(handle)(member)

    def localDisjunction[BR, U, C, B](getter: C => B)(implicit m1: Member.Aux[C \/ ?, R, U], m2: Member.Aux[B \/  ?, BR, U]): Eff[BR, A] =
      DisjunctionInterpretation.localDisjunction[R, BR, U, C, B, A](e, getter)

    def runLocalDisjunction[U, C, B](getter: C => B)(implicit sr: Member.Aux[C \/ ?, R, U], br: (B \/ ?) |= U): Eff[U, A] =
      DisjunctionInterpretation.runLocalDisjunction[R, U, C, B, A](e, getter)

  }

}
