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
  def left[R, E, A](e: E)(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
    send[E \/ ?, R, A](-\/(e))

  /** create a correct value */
  def right[R, E, A](a: A)(implicit member: Member[(E \/ ?), R]): Eff[R, A] =
    send[E \/ ?, R, A](\/-(a))

  /** run the disjunction effect, yielding E \/ A */
  def runDisjunction[R <: Effects, E, A](r: Eff[(E \/ ?) |: R, A]): Eff[R, E \/ A] = {
    val recurse = new Recurse[(E \/ ?), R, E \/ A] {
      def apply[X](m: E \/ X) =
        m match {
          case -\/(e) => \/-(EffMonad[R].point(-\/(e)))
          case \/-(a) => -\/(a)
        }
    }

    interpret1[R, (E \/ ?), A, E \/ A]((a: A) => \/-(a))(recurse)(r)
  }

  /** run the disjunction effect, yielding Either[E, A] */
  def runDisjunctionEither[R <: Effects, E, A](r: Eff[(E \/ ?) |: R, A]): Eff[R, Either[E, A]] =
    runDisjunction(r).map(_.fold(Left.apply, Right.apply))

}
