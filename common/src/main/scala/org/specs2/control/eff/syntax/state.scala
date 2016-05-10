package org.specs2.control.eff.syntax

import scalaz._
import org.specs2.control.eff._

object state extends state

trait state {

  implicit class StateEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runState[S, U <: Effects](s: S)(implicit member: Member.Aux[({type l[X]=State[S, X]})#l, R, U]): Eff[U, (A, S)] =
      StateInterpretation.runState(s)(e)

    def runStateTagged[S, U <: Effects, T](s: S)(implicit member: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, (A, S)] =
      StateInterpretation.runStateTagged(s)(e)

    def runStateZero[S : Monoid, U <: Effects](implicit member: Member.Aux[({type l[X]=State[S, X]})#l, R, U]): Eff[U, (A, S)] =
      StateInterpretation.runStateZero(e)

    def evalState[S, U <: Effects](s: S)(implicit member: Member.Aux[({type l[X]=State[S, X]})#l, R, U]): Eff[U, A] =
      StateInterpretation.evalState(s)(e)

    def evalStateTagged[S, U <: Effects, T](s: S)(implicit member: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, A] =
      StateInterpretation.evalStateTagged(s)(e)

    def evalStateZero[S : Monoid, U <: Effects](implicit member: Member.Aux[({type l[X]=State[S, X]})#l, R, U]): Eff[U, A] =
      StateInterpretation.evalStateZero(e)

    def execState[S, U <: Effects](s: S)(implicit member: Member.Aux[({type l[X]=State[S, X]})#l, R, U]): Eff[U, S] =
      StateInterpretation.execState(s)(e)

    def execStateZero[S : Monoid, U <: Effects](implicit member: Member.Aux[({type l[X]=State[S, X]})#l, R, U]): Eff[U, S] =
      StateInterpretation.execStateZero(e)

    def execStateTagged[S, U <: Effects, T](s: S)(implicit member: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, S] =
      StateInterpretation.execStateTagged(s)(e)

  }

}



