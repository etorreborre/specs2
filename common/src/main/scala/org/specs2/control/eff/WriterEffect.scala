package org.specs2.control
package eff

import scala.collection.mutable._
import all._
import interpret._
import syntax.all._
import origami._
import org.specs2.fp._
import org.specs2.fp.syntax._

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
  def runWriterFold[R, U, O, A, B](w: Eff[R, A])(fold: FoldId[O, B])(implicit m: Member.Aux[Writer[O, ?], R, U]): Eff[U, (A, B)] = {
    val recurse: StateRecurse[Writer[O, ?], A, (A, B)] = new StateRecurse[Writer[O, ?], A, (A, B)] {
      type S = fold.S
      val init = fold.start.run

      def apply[X](x: Writer[O, X], s: S) = (x.run._2, fold.fold(s, x.run._1))

      def applicative[X, T[_] : Traverse](xs: T[Writer[O, X]], s: S): (T[X], S) Either (Writer[O, T[X]], S) = {
        val os = new collection.mutable.ListBuffer[O]
        val values = xs.map { w: Writer[O, X] =>
          val (o, x) = w.run
          os.append(o)
          x
        }
        Left((values, os.toList.foldLeft(s) { (res, cur) => fold.fold(res, cur) }))
      }

      def finalize(a: A, s: S) = (a, fold.end(s).run)
    }

    interpretState1[R, U, Writer[O, ?], A, (A, B)]((a: A) => (a, fold.end(fold.start.run).run))(recurse)(w)
  }

  /**
   * Run a side-effecting fold
   */
  def runWriterUnsafe[R, U, O, A](w: Eff[R, A])(f: O => Unit)(implicit m: Member.Aux[Writer[O, ?], R, U]): Eff[U, A] =
    runWriterFold(w)(UnsafeFold(f)).map(_._1)

  implicit def ListFold[A]: FoldId[A, List[A]] = new Fold[NoFx, A, List[A]] {
    type S = ListBuffer[A]
    val start = pure[NoFx, ListBuffer[A]](new ListBuffer[A])
    def fold = (s: S, a: A) => { s.append(a); s }
    def end(s: S) = pure(s.toList)
  }

  def MonoidFold[A : Monoid]: FoldId[A, A] = new Fold[NoFx, A, A] {
    type S = A
    val start = pure[NoFx, A](Monoid[A].zero)
    def fold = (s: S, a: A) => a append s
    def end(s: S) = pure(s)
  }

  def UnsafeFold[A](f: A => Unit): FoldId[A, Unit] = new Fold[NoFx, A, Unit] {
    type S = Unit
    val start = pure[NoFx, Unit](())
    def fold = (s: S, a: A) => f(a)
    def end(s: S) = pure(s)
  }

}

object WriterInterpretation extends WriterInterpretation
