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
  def optionDisjunction[R, E, A](option: Option[A], e: E)(implicit member: Member[({type l[X]=(E \/ X)})#l, R]): Eff[R, A] =
    option.fold[Eff[R, A]](left[R, E, A](e))(right[R, E, A])

  /** create an disjunction effect from a single Disjunction value */
  def fromDisjunction[R, E, A](disjunction: E \/ A)(implicit member: Member[({type l[X]=(E \/ X)})#l, R]): Eff[R, A] =
    disjunction.fold[Eff[R, A]](left[R, E, A], right[R, E, A])

  /** create a failed value */
  def left[R, E, A](e: E)(implicit member: Member[({type l[X]=(E \/ X)})#l, R]): Eff[R, A] =
    send[({type l[X]=(E \/ X)})#l, R, A](-\/(e))

  /** create a correct value */
  def right[R, E, A](a: A)(implicit member: Member[({type l[X]=(E \/ X)})#l, R]): Eff[R, A] =
    send[({type l[X]=(E \/ X)})#l, R, A](\/-(a))

}

object DisjunctionCreation extends DisjunctionCreation

trait DisjunctionInterpretation {

  /** run the disjunction effect, yielding E \/ A */
  def runDisjunction[R <: Effects, U <: Effects, E, A](r: Eff[R, A])(implicit member: Member.Aux[({type l[X]=(E \/ X)})#l, R, U]): Eff[U, E \/ A] = {
    val recurse = new Recurse[({type l[X]=(E \/ X)})#l, U, E \/ A] {
      def apply[X](m: E \/ X) =
        m match {
          case -\/(e) => \/.right(EffMonad[U].point(\/.left[E, A](e)))
          case \/-(a) => \/.left(a)
        }
    }


    interpret1[R, U, ({type l[X]=(E \/ X)})#l, A, E \/ A]((a: A) => \/.right[E, A](a))(recurse)(r)
  }

  /** run the disjunction effect, yielding Either[E, A] */
  def runDisjunctionEither[R <: Effects, U <: Effects, E, A](r: Eff[R, A])(implicit member: Member.Aux[({type l[X]=(E \/ X)})#l, R, U]): Eff[U, Either[E, A]] =
    runDisjunction(r).map(_.fold(Left.apply, Right.apply))

}

object DisjunctionInterpretation extends DisjunctionInterpretation
