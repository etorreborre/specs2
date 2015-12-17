package org.specs2.control.eff

import Eff._
import Effects._
import Interpret._

import scala.collection.mutable._
import scalaz._, Scalaz._

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
object WriterEffect {

  /** write a give value */
  def tell[R, O](o: O)(implicit member: Member[Writer[O, ?], R]): Eff[R, Unit] =
    send[Writer[O, ?], R, Unit](Writer(o, ()))

  /**
   * run a writer effect and return the list of written values
   *
   * This uses a ListBuffer internally to append values
   */
  def runWriter[R <: Effects, O, A, B](w: Eff[Writer[O, ?] |: R, A]): Eff[R, (A, List[O])] =
    runWriterFold(w)(ListFold)

  /**
   * More general fold of runWriter where we can use a fold to accumulate values in a mutable buffer
   */
  def runWriterFold[R <: Effects, O, A, B](w: Eff[Writer[O, ?] |: R, A])(implicit fold: Fold[O, B]): Eff[R, (A, B)] = {
    val recurse: StateRecurse[Writer[O, ?], A, (A, B)] = new StateRecurse[Writer[O, ?], A, (A, B)] {
      type S = fold.S
      val init = fold.init
      def apply[X](x: Writer[O, X], s: S) = (x.run._2, fold.fold(x.run._1, s))
      def finalize(a: A, s: S) = (a, fold.finalize(s))
    }

    interpretState1[R, Writer[O, ?], A, (A, B)]((a: A) => (a, fold.finalize(fold.init)))(recurse)(w)
  }

  /**
   * run a tagged writer effect
   */
  def runTaggedWriter[R <: Effects, T, O, A](w: Eff[({type l[X] = Writer[O, X] @@ T})#l |: R, A]): Eff[R, (A, List[O])] =
    runTaggedWriterFold(w)(ListFold)

  def runTaggedWriterFold[R <: Effects, T, O, A, B](w: Eff[({type l[X] = Writer[O, X] @@ T})#l |: R, A])(implicit fold: Fold[O, B]): Eff[R, (A, B)] = {
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

    interpretState1[R, W, A, (A, B)]((a: A) => (a, fold.finalize(fold.init)))(recurse)(w)
  }

  /** support trait for folding values while possibly keeping some internal state */
  trait Fold[A, B] {
    type S
    val init: S
    def fold(a: A, s: S): S
    def finalize(s: S): B
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
