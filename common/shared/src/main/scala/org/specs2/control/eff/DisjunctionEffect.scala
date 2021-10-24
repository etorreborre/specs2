package org.specs2.control.eff

import org.specs2.fp._
import org.specs2.fp.syntax._
import Eff._
import Interpret._

/**
 * Effect for computation which can fail
 */
trait DisjunctionEffect extends
        DisjunctionCreation with
        DisjunctionInterpretation

object DisjunctionEffect extends DisjunctionEffect

trait DisjunctionCreation {

  type ThrowableOr[A] = Throwable Either A
  type _ThrowableOr[R] = ThrowableOr <= R
  type _throwableOr[R] = ThrowableOr |= R

  /** create an Either effect from a single Option value */
  def optionDisjunction[R, E, A](option: Option[A], e: E)(implicit member: Either[E, *] |= R): Eff[R, A] =
    option.fold[Eff[R, A]](left[R, E, A](e))(right[R, E, A])

  /** create an Either effect from a single Either value */
  def fromDisjunction[R, E, A](disjunction: E Either A)(implicit member: Either[E, *] |= R): Eff[R, A] =
    disjunction.fold[Eff[R, A]](left[R, E, A], right[R, E, A])

  /** create a failed value */
  def left[R, E, A](e: E)(implicit member: Either[E, *] |= R): Eff[R, A] =
    send[Either[E, *], R, A](Left(e))

  /** create a correct value */
  def right[R, E, A](a: A)(implicit member: Either[E, *] |= R): Eff[R, A] =
    send[Either[E, *], R, A](Right(a))

}

object DisjunctionCreation extends DisjunctionCreation

trait DisjunctionInterpretation {

  /** run the disjunction effect, yielding E Either A */
  def runDisjunction[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[(Either[E, *]), R, U]): Eff[U, E Either A] = {
    val recurse = new Recurse[(Either[E, *]), U, E Either A] {
      def apply[X](m: E Either X) =
        m match {
          case Left(e) => Right(EffMonad[U].point(Left(e)))
          case Right(a) => Left(a)
        }

      def applicative[X, T[_] : Traverse](ms: T[E Either X]): T[X] Either (E Either T[X]) =
        Right(ms.sequence)
    }

    interpret1[R, U, (Either[E, *]), A, E Either A]((a: A) => Right(a): E Either A)(recurse)(r)
  }

  /** run the disjunction effect, yielding Either[E, A] */
  def runEither[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[(Either[E, *]), R, U]): Eff[U, Either[E, A]] =
    runDisjunction(r).map(_.fold(util.Left.apply, util.Right.apply))

  /** catch and handle a possible Left value */
  def catchLeft[R, E, A](r: Eff[R, A])(handle: E => Eff[R, A])(implicit member: (Either[E, *]) <= R): Eff[R, A] = {
    val recurse = new Recurse[(Either[E, *]), R, A] {
      def apply[X](m: E Either X) =
        m match {
          case Left(e) => Right(handle(e))
          case Right(a) => Left(a)
        }

      def applicative[X, T[_] : Traverse](ms: T[E Either X]): T[X] Either (E Either T[X]) =
        Right(ms.sequence)
    }

    intercept1[R, (Either[E, *]), A, A]((a: A) => a)(recurse)(r)
  }

  /**
   * Translate an error effect to another one in the same stack
   * a computation over a "bigger" error (for the full application)
   */
  def runLocalDisjunction[R, U, E1, E2, A](r: Eff[R, A], getter: E1 => E2)
                                  (implicit sr: Member.Aux[Either[E1, *], R, U], br: Either[E2, *] |= U): Eff[U, A] =
    translate(r) { new Translate[Either[E1, *], U] {
      def apply[X](ex: E1 Either X): Eff[U, X] =
        ex match {
          case Left(e1) => DisjunctionEffect.left[U, E2, X](getter(e1))
          case Right(x)  => pure(x)
        }
    }}

}

object DisjunctionInterpretation extends DisjunctionInterpretation
