package org.specs2.control.eff

import scalaz._
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

  /** create an disjunction effect from a single Option value */
  def optionDisjunction[R, E, A](option: Option[A], e: E)(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
    option.fold[Eff[R, A]](left[R, E, A](e))(right[R, E, A])

  /** create an disjunction effect from a single Disjunction value */
  def fromDisjunction[R, E, A](disjunction: E \/ A)(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
    disjunction.fold[Eff[R, A]](left[R, E, A], right[R, E, A])

  /** create a failed value */
  def left[R, E, A](e: E)(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
    send[E \/ ?, R, A](-\/(e))

  /** create a correct value */
  def right[R, E, A](a: A)(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
    send[E \/ ?, R, A](\/-(a))

}

object DisjunctionCreation extends DisjunctionCreation

trait DisjunctionInterpretation {

  /** run the disjunction effect, yielding E \/ A */
  def runDisjunction[R <: Effects, U <: Effects, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E \/ ?), R, U]): Eff[U, E \/ A] = {
    val recurse = new Recurse[(E \/ ?), U, E \/ A] {
      def apply[X](m: E \/ X) =
        m match {
          case -\/(e) => \/.right(EffMonad[U].point(-\/(e)))
          case \/-(a) => \/.left(a)
        }
    }

    interpret1[R, U, (E \/ ?), A, E \/ A]((a: A) => \/-(a): E \/ A)(recurse)(r)
  }

  /** run the disjunction effect, yielding Either[E, A] */
  def runEither[R <: Effects, U <: Effects, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E \/ ?), R, U]): Eff[U, Either[E, A]] =
    runDisjunction(r).map(_.fold(util.Left.apply, util.Right.apply))

  /** catch and handle a possible left value */
  def catchLeft[R <: Effects, E, A](r: Eff[R, A])(handle: E => Eff[R, A])(implicit member: Member[(E \/ ?), R]): Eff[R, A] = {
    val recurse = new Recurse[(E \/ ?), R, A] {
      def apply[X](m: E \/ X) =
        m match {
          case -\/(e) => \/-(handle(e))
          case \/-(a) => -\/(a)
        }
    }

    intercept1[R, (E \/ ?), A, A]((a: A) => a)(recurse)(r)
  }

}

object DisjunctionInterpretation extends DisjunctionInterpretation
