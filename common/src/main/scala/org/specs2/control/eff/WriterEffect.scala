package org.specs2.control.eff

import scala.collection.mutable._
import scalaz._
import scalaz.syntax.semigroup._
import Eff._
import Interpret._

/**
 * Effect for logging values alongside computations
 *
 * Compared to traditional Writer monad which accumulates values by default
 * this effect can be interpreted in different ways:
 *
 *  - log values to the console or to a file as soon as they are produced
 *  - accumulate values in a list
 *
 * Several writer effects can be used in the same stack if they are tagged.
 *
 */
trait WriterEffect extends
  WriterCreation with
  WriterInterpretation

object WriterEffect extends WriterEffect

trait WriterCreation {

  /** write a given value */
  def tell[R, O](o: O)(implicit member: Member[({type l[X]=Writer[O, X]})#l, R]): Eff[R, Unit] =
    send[({type l[X]=Writer[O, X]})#l, R, Unit](Writer(o, ()))

  /** write a given value */
  def tellTagged[R, T, O](o: O)(implicit member: Member[({type l[X] = Writer[O, X] @@ T})#l, R]): Eff[R, Unit] =
    send[({type l[X] = Writer[O, X] @@ T})#l, R, Unit](Tag(Writer(o, ())))

}

object WriterCreation extends WriterCreation

trait WriterInterpretation {

  /**
   * run a writer effect and return the list of written values
   *
   * This uses a ListBuffer internally to append values
   */
  def runWriter[R <: Effects, U <: Effects, O, A, B](w: Eff[R, A])(implicit m: Member.Aux[({type l[X]=Writer[O, X]})#l, R, U]): Eff[U, (A, List[O])] =
    runWriterFold(w)(ListFold)

  /**
   * More general fold of runWriter where we can use a fold to accumulate values in a mutable buffer
   */
  def runWriterFold[R <: Effects, U <: Effects, O, A, B](w: Eff[R, A])(fold: Fold[O, B])(implicit m: Member.Aux[({type l[X]=Writer[O, X]})#l, R, U]): Eff[U, (A, B)] = {
    val recurse: StateRecurse[({type l[X]=Writer[O, X]})#l, A, (A, B)] = new StateRecurse[({type l[X]=Writer[O, X]})#l, A, (A, B)] {
      type S = fold.S
      val init = fold.init
      def apply[X](x: Writer[O, X], s: S) = (x.run._2, fold.fold(x.run._1, s))
      def finalize(a: A, s: S) = (a, fold.finalize(s))
    }

    interpretState1[R, U, ({type l[X]=Writer[O, X]})#l, A, (A, B)]((a: A) => (a, fold.finalize(fold.init)))(recurse)(w)
  }

  /**
   * run a tagged writer effect
   */
  def runWriterTagged[R <: Effects, U <: Effects, T, O, A](w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = Writer[O, X] @@ T})#l, R, U]): Eff[U, (A, List[O])] =
    runTaggedWriterFold(w)(ListFold)

  def runTaggedWriterFold[R <: Effects, U <: Effects, T, O, A, B](w: Eff[R, A])(fold: Fold[O, B])(implicit
        m: Member.Aux[({type l[X] = Writer[O, X] @@ T})#l, R, U]): Eff[U, (A, B)] = {
    type W[X] = Writer[O, X] @@ T

    val recurse = new StateRecurse[W, A, (A, B)] {
      type S = fold.S
      val init = fold.init

      def apply[X](xt: W[X], s: S): (X, S) =
        Tag.unwrap(xt) match {
          case x => (x.run._2, fold.fold(x.run._1, s))
        }

      def finalize(a: A, s: S): (A, B) =
        (a, fold.finalize(s))
    }

    interpretState1[R, U, W, A, (A, B)]((a: A) => (a, fold.finalize(fold.init)))(recurse)(w)
  }

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
}

/** support trait for folding values while possibly keeping some internal state */
trait Fold[A, B] {
  type S
  val init: S
  def fold(a: A, s: S): S
  def finalize(s: S): B
}

object WriterInterpretation extends WriterInterpretation
