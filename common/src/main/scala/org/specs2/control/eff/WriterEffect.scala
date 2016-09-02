package org.specs2.control.eff

import scala.collection.mutable._
import scalaz._, Scalaz._
import Eff._
import Interpret._
import EvalEffect._

/**
 * Effect for logging values alongside computations
 *
 * Compared to traditional Writer monad which accumulates values by default
 * this effect can be interpreted in different ways:
 *
 *  - log values to the console or to a file as soon as they are produced
 *  - accumulate values in a list
 *
 */
trait WriterEffect extends
  WriterCreation with
  WriterInterpretation

object WriterEffect extends WriterEffect

trait WriterCreation {

  /** write a given value */
  def tell[R, O](o: O)(implicit member: Writer[O, ?] |= R): Eff[R, Unit] =
    send[Writer[O, ?], R, Unit](Writer(o, ()))

}

object WriterCreation extends WriterCreation

trait WriterInterpretation {

  /**
   * run a writer effect and return the list of written values
   *
   * This uses a ListBuffer internally to append values
   */
  def runWriter[R, U, O, A, B](w: Eff[R, A])(implicit m: Member.Aux[Writer[O, ?], R, U]): Eff[U, (A, List[O])] =
    runWriterFold(w)(ListFold)

  /**
   * More general fold of runWriter where we can use a fold to accumulate values in a mutable buffer
   */
  def runWriterFold[R, U, O, A, B](w: Eff[R, A])(fold: Fold[O, B])(implicit m: Member.Aux[Writer[O, ?], R, U]): Eff[U, (A, B)] = {
    val recurse: StateRecurse[Writer[O, ?], A, (A, B)] = new StateRecurse[Writer[O, ?], A, (A, B)] {
      type S = fold.S
      val init = fold.init
      def apply[X](x: Writer[O, X], s: S) = (x.run._2, fold.fold(x.run._1, s))
      def finalize(a: A, s: S) = (a, fold.finalize(s))
    }

    interpretState1[R, U, Writer[O, ?], A, (A, B)]((a: A) => (a, fold.finalize(fold.init)))(recurse)(w)
  }

  /**
   * Run a side-effecting fold
   */
  def runWriterUnsafe[R, U, O, A](w: Eff[R, A])(f: O => Unit)(implicit m: Member.Aux[Writer[O, ?], R, U]): Eff[U, A] =
    runWriterFold(w)(UnsafeFold(f)).map(_._1)

  def runWriterEval[R, U, O, A](w: Eff[R, A])(f: O => Eval[Unit])(implicit m: Member.Aux[Writer[O, ?], R, U], ev: Eval |= U): Eff[U, A] =
    runWriterFold(w)(EvalFold(f)).flatMap { case (a, e) => send[Eval, U, Unit](e).as(a) }

  implicit def ListFold[A]: Fold[A, List[A]] = new Fold[A, List[A]] {
    type S = ListBuffer[A]
    val init = new ListBuffer[A]
    def fold(a: A, s: S) = { s.append(a); s }
    def finalize(s: S) = s.toList
  }

  def MonoidFold[A : Monoid]: Fold[A, A] = new Fold[A, A] {
    type S = A
    val init = Monoid[A].zero
    def fold(a: A, s: S) = a |+| s
    def finalize(s: S) = s
  }

  def UnsafeFold[A](f: A => Unit): Fold[A, Unit] = new Fold[A, Unit] {
    type S = Unit
    val init = ()
    def fold(a: A, s: S) = f(a)
    def finalize(s: S) = s
  }

  def EvalFold[A](f: A => Eval[Unit]): Fold[A, Eval[Unit]] = new Fold[A, Eval[Unit]] {
    type S = Eval[Unit]
    val init = Name(())
    def fold(a: A, s: S) = s >> f(a)
    def finalize(s: S) = s
  }

}

/** support trait for folding values while possibly keeping some internal state */
trait Fold[A, B] {
  type S
  val init: S
  def fold(a: A, s: S): S
  def finalize(s: S): B
}

object WriterInterpretation extends WriterInterpretation
