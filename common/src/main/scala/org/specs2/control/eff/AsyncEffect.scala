package org.specs2.control
package eff

import scalaz._, Scalaz._
import all._

trait AsyncEffect {

  type _async[R] = Async |= R
  type _Async[R] = Async <= R

}

trait AsyncInterpretation {

  def asyncAttempt[R, A](e: Eff[R, A])(implicit async: Async /= R): Eff[R, Throwable Either A] = {
    e match {
      case Pure(a, last) =>
        pure[R, Throwable Either A](Right(a)).addLast(last)

      case Impure(u, c, last) =>
        async.extract(u) match {
          case Some(tx) =>
            val union = async.inject(tx.attempt)

            Impure(union, Arrs.singleton { ex: (Throwable Either u.X) =>
              ex match {
                case Right(x) => asyncAttempt(c(x))
                case Left(t) => pure(Left(t))
              }
            }, last)

          case None => Impure(u, Arrs.singleton((x: u.X) => asyncAttempt(c(x))), last)
        }

      case ImpureAp(unions, continuation, last) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          async.extract(u) match {
            case Some(tx) => async.inject(tx.attempt)
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val collected = unions.extract(async)
        val continuation1 = Arrs.singleton[R, List[Any], Throwable Either A] { ls: List[Any] =>
          val xors =
            ls.zipWithIndex.collect { case (a, i) =>
              if (collected.indices.contains(i)) a.asInstanceOf[Throwable Either Any]
              else Right(a)
            }.sequence

          xors match {
            case Left(t)     => pure(Left(t))
            case Right(anys) => asyncAttempt(continuation(anys))
          }
        }

        ImpureAp(materializedUnions, continuation1, last)
    }
  }

  implicit class AttemptOps[R, A](e: Eff[R, A])(implicit async: Async /= R){
    def asyncAttempt: Eff[R, Throwable Either A] =
      AsyncInterpretation.asyncAttempt(e)
  }
}

object AsyncInterpretation extends AsyncInterpretation

trait Async[+A] {
  def attempt: Async[Throwable Either A]
}
