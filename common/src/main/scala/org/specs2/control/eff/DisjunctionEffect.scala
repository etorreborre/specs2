package org.specs2.control.eff

import Eff._
import Effects._
import Interpret._

import scalaz._

/**
 * Effect for computation which can fail
 */
object DisjunctionEffect {

  /** create a failed value */
  def left[R, E, A](e: E)(implicit member: Member[({type l[X]=(E \/ X)})#l, R]): Eff[R, A] =
    send[({type l[X]=(E \/ X)})#l, R, A](-\/(e))

  /** create a correct value */
  def right[R, E, A](a: A)(implicit member: Member[({type l[X]=(E \/ X)})#l, R]): Eff[R, A] =
    send[({type l[X]=(E \/ X)})#l, R, A](\/-(a))

  /** run the disjunction effect, yielding E \/ A */
  def runDisjunction[R <: Effects, E, A](r: Eff[({type l[X]=(E \/ X)})#l |: R, A]): Eff[R, E \/ A] = {
    val recurse = new Recurse[({type l[X]=(E \/ X)})#l, R, E \/ A] {
      def apply[X](m: E \/ X) =
        m match {
          case -\/(e) => \/-(EffMonad[R].point(-\/(e)))
          case \/-(a) => -\/(a)
        }
    }

    interpret1[R, ({type l[X]=(E \/ X)})#l, A, E \/ A]((a: A) => \/-(a))(recurse)(r)
  }

  /** run the disjunction effect, yielding Either[E, A] */
  def runDisjunctionEither[R <: Effects, E, A](r: Eff[({type l[X]=(E \/ X)})#l |: R, A]): Eff[R, Either[E, A]] =
    runDisjunction(r).map(_.fold(Left.apply, Right.apply))

}
