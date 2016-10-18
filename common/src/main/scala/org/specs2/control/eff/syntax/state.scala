package org.specs2.control.eff.syntax

import scalaz._
import org.specs2.control.eff._

object state extends state

trait state {

  implicit class StateEffectOps[R, A](e: Eff[R, A]) {

    def runState[S](s: S)(implicit member: Member[State[S, ?], R]): Eff[member.Out, (A, S)] =
      StateInterpretation.runState(s)(e)(member.aux)

    def runStateZero[S : Monoid](implicit member: Member[State[S, ?], R]): Eff[member.Out, (A, S)] =
      StateInterpretation.runStateZero(e)(Monoid[S], member.aux)

    def evalState[S](s: S)(implicit member: Member[State[S, ?], R]): Eff[member.Out, A] =
      StateInterpretation.evalState(s)(e)(member.aux)

    def evalStateZero[S : Monoid](implicit member: Member[State[S, ?], R]): Eff[member.Out, A] =
      StateInterpretation.evalStateZero(e)(Monoid[S], member.aux)

    def execState[S](s: S)(implicit member: Member[State[S, ?], R]): Eff[member.Out, S] =
      StateInterpretation.execState(s)(e)(member.aux)

    def execStateZero[S : Monoid](implicit member: Member[State[S, ?], R]): Eff[member.Out, S] =
      StateInterpretation.execStateZero(e)(Monoid[S], member.aux)

    def lensState[BR, U, T, S](getter: S => T, setter: (S, T) => S)(implicit m1: Member.Aux[State[T, ?], R, U], m2: Member.Aux[State[S, ?], BR, U]): Eff[BR, A] =
      StateInterpretation.lensState[R, BR, U, T, S, A](e, getter, setter)

  }

}



