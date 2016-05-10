package org.specs2.control.eff

import scalaz.syntax.monad._
import scalaz._
import Interpret._
import Eff._
import Effects.|:

/**
 * Effect for passing state along computations
 *
 * Several state effects can be used in the same stack if they are tagged
 *
 * Internally backed up by scalaz.State
 *
 */
trait StateEffect extends
  StateCreation with
  StateInterpretation with
  StateImplicits

object StateEffect extends StateEffect

trait StateCreation {

  /** store a new state value */
  def put[R, S](s: S)(implicit member: Member[({type l[X]=State[S, X]})#l, R]): Eff[R, Unit] =
    send[({type l[X]=State[S, X]})#l, R, Unit](State.put(s))

  /** get the current state value */
  def get[R, S](implicit member: Member[({type l[X]=State[S, X]})#l, R]): Eff[R, S] =
    send[({type l[X]=State[S, X]})#l, R, S](State.get)

  /** get the current state value and map it with a function f */
  def gets[R, S, T](f: S => T)(implicit member: Member[({type l[X]=State[S, X]})#l, R]): Eff[R, T] =
    send[({type l[X]=State[S, X]})#l, R, T](State.gets(f))

  /** modify the current state value */
  def modify[R, S](f: S => S)(implicit member: Member[({type l[X]=State[S, X]})#l, R]): Eff[R, Unit] =
    get >>= ((s: S) => put(f(s)))

  /** store a new state value */
  def putTagged[R, T, S](s: S)(implicit member: Member[({type l[X] = State[S, X] @@ T})#l, R]): Eff[R, Unit] =
    send[({type l[X] = State[S, X] @@ T})#l, R, Unit](Tag(State.put(s)))

  /** get the current state value */
  def getTagged[R, T, S](implicit member: Member[({type l[X] = State[S, X] @@ T})#l, R]): Eff[R, S] =
    send[({type l[X] = State[S, X] @@ T})#l, R, S](Tag(State.get))

  /** get the current state value and map it with a function f */
  def getsTagged[R, U, S, T](f: S => T)(implicit member: Member[({type l[X] = State[S, X] @@ U})#l, R]): Eff[R, T] =
    send[({type l[X] = State[S, X] @@ U})#l, R, T](Tag(State.gets(f)))

  /** modify the current state value */
  def modifyTagged[R, T, S](f: S => S)(implicit member: Member[({type l[X] = State[S, X] @@ T})#l, R]): Eff[R, Unit] =
    getTagged >>= ((s: S) => putTagged(f(s)))

}

object StateCreation extends StateCreation

trait StateInterpretation {
  /** run a state effect, with a Monoidal state */
  def evalStateZero[R <: Effects, U <: Effects, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[({type l[X]=State[S, X]})#l, R, U]): Eff[U, A] =
    evalState(Monoid[S].zero)(w)

  /** run a state effect, with an initial value, return only the value */
  def evalState[R <: Effects, U <: Effects, S, A](initial: S)(w: Eff[R, A])(implicit m: Member.Aux[({type l[X]=State[S, X]})#l, R, U]): Eff[U, A] =
    runState(initial)(w).map(_._1)

  /** run a state effect, with a monoidal state, return only the state */
  def execStateZero[R <: Effects, U <: Effects, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[({type l[X]=State[S, X]})#l, R, U]): Eff[U, S] =
    execState(Monoid[S].zero)(w)

  /** run a state effect, with an initial value, return only the state */
  def execState[R <: Effects, U <: Effects, S, A](initial: S)(w: Eff[R, A])(implicit m: Member.Aux[({type l[X]=State[S, X]})#l, R, U]): Eff[U, S] =
    runState(initial)(w).map(_._2)

  /** run a state effect, with an initial value */
  def runStateZero[R <: Effects, U <: Effects, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[({type l[X]=State[S, X]})#l, R, U]): Eff[U, (A, S)] =
    runState(Monoid[S].zero)(w)

  /** run a state effect, with an initial value */
  def runState[R <: Effects, U <: Effects, S1, A](initial: S1)(w: Eff[R, A])(implicit m: Member.Aux[({type l[X]=State[S1, X]})#l, R, U]): Eff[U, (A, S1)] = {
    val recurse: StateRecurse[({type l[X]=State[S1, X]})#l, A, (A, S1)] = new StateRecurse[({type l[X]=State[S1, X]})#l, A, (A, S1)] {

      type S = S1
      val init = initial

      def apply[X](x: State[S, X], s: S) =
        x.run(s).swap

      def finalize(a: A, s: S) = (a, s)

    }
    interpretState1[R, U, ({type l[X]=State[S1, X]})#l, A, (A, S1)]((a: A) => (a, initial))(recurse)(w)
  }

  /** run a state effect, with a Monoidal state */
  def evalStateZeroTagged[R <: Effects, U <: Effects, T, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, A] =
    evalStateTagged(Monoid[S].zero)(w)

  /** run a state effect, with an initial value, return only the value */
  def evalStateTagged[R <: Effects, U <: Effects, T, S, A](initial: S)(w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, A] =
    runStateTagged(initial)(w).map(_._1)

  /** run a state effect, with a monoidal state, return only the state */
  def execStateZeroTagged[R <: Effects, U <: Effects, T, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, S] =
    execStateTagged(Monoid[S].zero)(w)

  /** run a state effect, with an initial value, return only the state */
  def execStateTagged[R <: Effects, U <: Effects, T, S, A](initial: S)(w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, S] =
    runStateTagged(initial)(w).map(_._2)

  /** run a state effect, with an initial value */
  def runStateZeroTagged[R <: Effects, U <: Effects, T, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, (A, S)] =
    runStateTagged(Monoid[S].zero)(w)

  /** run a tagged state effect, with an initial value */
  def runStateTagged[R <: Effects, U <: Effects, T, S1, A](initial: S1)(w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = State[S1, X] @@ T})#l, R, U]): Eff[U, (A, S1)] = {
    type SS[X] = State[S1, X] @@ T

    val recurse: StateRecurse[SS, A, (A, S1)] = new StateRecurse[SS, A, (A, S1)] {
      type S = S1
      val init = initial

      def apply[X](x: State[S, X] @@ T, s: S) =
        Tag.unwrap(x).run(s).swap

      def finalize(a: A, s: S) = (a, s)

    }

    interpretState1[R, U, SS, A, (A, S1)]((a: A) => (a, initial))(recurse)(w)
  }

  /**
   * Lift a computation over a "small" state (for a subsystem) into
   * a computation over a "bigger" state (for the full application state)
   */
  def lensState[TS, SS, U, T, S, A](state: Eff[TS, A], getter: S => T, setter: (S, T) => S)
                                   (implicit ts: Member.Aux[({type l[X]=State[T, X]})#l, TS, U], ss: Member.Aux[({type l[X]=State[S, X]})#l, SS, U]): Eff[SS, A] =
    Interpret.transform[TS, SS, U, ({type l[X]=State[T, X]})#l, ({type l[X]=State[S, X]})#l, A](state, new ~>[({type l[X]=State[T, X]})#l, ({type l[X]=State[S, X]})#l] {
      def apply[X](tstate: State[T, X]): State[S, X] =
        State { s: S =>
          val (t, x) = tstate.run(getter(s))
          (setter(s, t), x)
        }
    })

}

object StateInterpretation extends StateInterpretation

trait StateImplicits extends StateImplicits1 {
  implicit def TaggedStateMemberZero[Tg, A]: Member.Aux[({type l[X] = State[A, X] @@ Tg})#l, ({type l[X] = State[A, X] @@ Tg})#l |: NoEffect, NoEffect] = {
    type T[X] = State[A, X] @@ Tg
    Member.zero[T]
  }

  implicit def TaggedStateMemberFirst[R <: Effects, Tg, A]: Member.Aux[({type l[X] = State[A, X] @@ Tg})#l, ({type l[X] = State[A, X] @@ Tg})#l |: R, R] = {
    type T[X] = State[A, X] @@ Tg
    Member.first[T, R]
  }

}
trait StateImplicits1 {
  implicit def TaggedStateMemberSuccessor[O[_], R <: Effects, U <: Effects, Tg, A](implicit m: Member.Aux[({type l[X] = State[A, X] @@ Tg})#l, R, U]): Member.Aux[({type l[X] = State[A, X] @@ Tg})#l, O |: R, O |: U] = {
    type T[X] = State[A, X] @@ Tg
    Member.successor[T, O, R, U]
  }

}

object StateImplicits extends StateImplicits

