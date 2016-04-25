package org.specs2.control.eff

import Eff._
import Effects._
import Interpret._

import scalaz.{-\/, \/-}

/**
 * Effect for optional computations
 */
object OptionEffect {

  /** no value returned */
  def none[R, A](implicit member: Member[Option, R]): Eff[R, A] =
    send[Option, R, A](None)

  /** a value is returned */
  def some[R, A](a: A)(implicit member: Member[Option, R]): Eff[R, A] =
    send[Option, R, A](Some(a))

  /**
   * Interpret the Option effect
   *
   * Stop all computations if None is present once
   */
  def runOption[R <: Effects, A](r: Eff[Option |: R, A]): Eff[R, Option[A]] = {
    val recurse = new Recurse[Option, R, Option[A]] {
      def apply[X](m: Option[X]) =
        m match {
          case None    => \/-(EffMonad[R].point(None))
          case Some(x) => -\/(x)
        }
    }

    interpret1[R, Option, A, Option[A]]((a: A) => Option(a))(recurse)(r)
  }
}

