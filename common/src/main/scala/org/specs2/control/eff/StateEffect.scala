package org.specs2.control.eff

import scalaz._, Scalaz._
import Interpret._
import Eff._

/**
 * Effect for passing state along computations
 *
 * Internally backed up by scalaz.State
 *
 */
trait StateEffect extends
  StateCreation with
  StateInterpretation

object StateEffect extends StateEffect

trait StateCreation {

  /** store a new state value */
  def put[R, S](s: S)(implicit member: State[S, ?] |= R): Eff[R, Unit] =
    send[State[S, ?], R, Unit](State.put(s))

  /** get the current state value */
  def get[R, S](implicit member: State[S, ?] |= R): Eff[R, S] =
    send[State[S, ?], R, S](State.get)

  /** get the current state value and map it with a function f */
  def gets[R, S, T](f: S => T)(implicit member: State[S, ?] |= R): Eff[R, T] =
    send[State[S, ?], R, T](State.gets(f))

  /** modify the current state value */
  def modify[R, S](f: S => S)(implicit member: State[S, ?] |= R): Eff[R, Unit] =
    get >>= ((s: S) => put(f(s)))

}

trait StateImplicits {

  implicit def stateMemberInToReaderMemberIn[E, S](implicit m: MemberIn[State[S, ?], E]): MemberIn[Reader[S, ?], E] =
    m.transform(readerToStateNat)

  implicit def stateMemberInLens[E, S, T](implicit m: MemberIn[State[S, ?], E], get: S => T, set: T => S => S): MemberIn[State[T, ?], E] =
    m.transform(via(get, set))

  def readerToStateNat[S1] = new (Reader[S1, ?] ~> State[S1, ?]) {
    def apply[X](r: Reader[S1, X]): State[S1, X] =
      State((s: S1) => (s, r.run(s)))
  }

  def via[S, T](get: S => T, set: T => S => S): (State[T, ?] ~> State[S, ?]) =
    new (State[T, ?] ~> State[S, ?]) {
      def apply[X](s: State[T, X]) =
        State[S, X] { s1 =>
          val (t, x) = s.run(get(s1)).value
          (set(t)(s1), x)
        }
    }

}

object StateImplicits extends StateImplicits

object StateCreation extends StateCreation

trait StateInterpretation {
  /** run a state effect, with a Monoidal state */
  def evalStateZero[R, U, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[State[S, ?], R, U]): Eff[U, A] =
    evalState(Monoid[S].zero)(w)

  /** run a state effect, with an initial value, return only the value */
  def evalState[R, U, S, A](initial: S)(w: Eff[R, A])(implicit m: Member.Aux[State[S, ?], R, U]): Eff[U, A] =
    runState(initial)(w).map(_._1)

  /** run a state effect, with a monoidal state, return only the state */
  def execStateZero[R, U, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[State[S, ?], R, U]): Eff[U, S] =
    execState(Monoid[S].zero)(w)

  /** run a state effect, with an initial value, return only the state */
  def execState[R, U, S, A](initial: S)(w: Eff[R, A])(implicit m: Member.Aux[State[S, ?], R, U]): Eff[U, S] =
    runState(initial)(w).map(_._2)

  /** run a state effect, with an initial value */
  def runStateZero[R, U, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[State[S, ?], R, U]): Eff[U, (A, S)] =
    runState(Monoid[S].zero)(w)

  /** run a state effect, with an initial value */
  def runState[R, U, S1, A](initial: S1)(w: Eff[R, A])(implicit m: Member.Aux[State[S1, ?], R, U]): Eff[U, (A, S1)] = {
    val recurse: StateRecurse[State[S1, ?], A, (A, S1)] = new StateRecurse[State[S1, ?], A, (A, S1)] {
      type S = S1
      val init = initial

      def apply[X](x: State[S, X], s: S) =
        x.run(s).value.swap

      def applicative[X, T[_] : Traverse](xs: T[State[S, X]], s: S): (T[X], S) \/ (State[S, T[X]], S) =
        -\/(xs.sequence.run(s).value.swap)

      def finalize(a: A, s: S) = (a, s)

    }

    interpretState1[R, U, State[S1, ?], A, (A, S1)]((a: A) => (a, initial))(recurse)(w)
  }

  /**
   * Lift a computation over a "small" state (for a subsystem) into
   * a computation over a "bigger" state (for the full application state)
   */
  def lensState[TS, SS, U, T, S, A](state: Eff[TS, A], getter: S => T, setter: (S, T) => S)
                                   (implicit ts: Member.Aux[State[T, ?], TS, U], ss: Member.Aux[State[S, ?], SS, U]): Eff[SS, A] =
  Interpret.transform[TS, SS, U, State[T, ?], State[S, ?], A](state, new ~>[State[T, ?], State[S, ?]] {
    def apply[X](tstate: State[T, X]): State[S, X] =
      State { s: S =>
        val (t, x) = tstate.run(getter(s)).value
        (setter(s, t), x)
      }
  })

}

object StateInterpretation extends StateInterpretation


