package org.specs2.control.eff

import Eff._
import Effects._
import Interpret._

import scalaz._

/**
 * Effect for computations depending on an environment.
 *
 * The inside datatype for this effect is scalaz.Reader
 *
 * Several Reader effects can be present in a given stack provided that they are tagged with scala.Tag.
 *
 * A tagged Reader effect can be run with runTaggedReader
 *
 */
object ReaderEffect {

  /** get the environment */
  def ask[R, I](implicit member: Member[Reader[I, ?], R]): Eff[R, I] =
    send[Reader[I, ?], R, I](Reader(identity _))

  /** interpret the Reader effect by providing an environment when required */
  def runReader[R <: Effects, A, B](env: A)(r: Eff[Reader[A, ?] |: R, B]): Eff[R, B] = {
    val recurse = new Recurse[Reader[A, ?], R, B] {
      def apply[X](m: Reader[A, X]) = -\/(env.asInstanceOf[X])
    }

    interpret1[R, Reader[A, ?], B, B]((b: B) => b)(recurse)(r)
  }

  /** interpret a tagged Reader effect by providing an environment when required */
  def runTaggedReader[R <: Effects, T, A, B](env: A)(r: Eff[({type l[X] = Reader[A, X] @@ T})#l |: R, B]): Eff[R, B] = {
    val recurse = new Recurse[({type l[X] = Reader[A, X] @@ T})#l, R, B] {
      def apply[X](m: Reader[A, X] @@ T) = -\/(env.asInstanceOf[X])
    }

    interpret1[R, ({type l[X] = Reader[A, X] @@ T})#l, B, B]((b: B) => b)(recurse)(r)
  }

}
