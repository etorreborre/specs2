package org.specs2.control.eff.syntax

import scalaz.\/
import org.specs2.control.eff._

object disjunction extends disjunction

trait disjunction {

  implicit class DisjunctionEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runDisjunction[E, U <: Effects](implicit member: Member.Aux[({type l[X]=(E \/ X)})#l, R, U]): Eff[U, E \/ A] =
      DisjunctionInterpretation.runDisjunction(e)

    def runEither[E, U <: Effects](implicit member: Member.Aux[({type l[X]=(E \/ X)})#l, R, U]): Eff[U, E Either A] =
      DisjunctionInterpretation.runDisjunctionEither(e)

  }

}
