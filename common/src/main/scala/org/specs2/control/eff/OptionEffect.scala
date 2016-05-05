package org.specs2.control.eff

import scalaz._
import Interpret._
import Eff._

/**
 * Effect for optional computations
 */
trait OptionEffect extends
  OptionCreation with
  OptionInterpretation

object OptionEffect extends OptionEffect

trait OptionCreation {

  /** create an Option effect from a single Option value */
  def fromOption[R, A](o: Option[A])(implicit member: Member[Option, R]): Eff[R, A] =
    send[Option, R, A](o)

  /** no value returned */
  def none[R, A](implicit member: Member[Option, R]): Eff[R, A] =
    send[Option, R, A](None)

  /** a value is returned */
  def some[R, A](a: A)(implicit member: Member[Option, R]): Eff[R, A] =
    send[Option, R, A](Some(a))
}

object OptionCreation extends OptionCreation

trait OptionInterpretation {
  /**
   * Interpret the Option effect
   *
   * Stop all computations if None is present once
   */
  def runOption[R <: Effects, U <: Effects, A](r: Eff[R, A])(implicit m: Member.Aux[Option, R, U]): Eff[U, Option[A]] = {
    val recurse = new Recurse[Option, U, Option[A]] {
      def apply[X](m: Option[X]) =
        m match {
          case None    => \/-(EffMonad[U].point(None))
          case Some(x) => -\/(x)
        }
    }

    interpret1[R, U, Option, A, Option[A]]((a: A) => Option(a))(recurse)(r)
  }

}

object OptionInterpretation extends OptionInterpretation

