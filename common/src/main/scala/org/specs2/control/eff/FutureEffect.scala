package org.specs2.control.eff

import scala.util.control.NonFatal
import scalaz._, Scalaz._
import Eff._
import Interpret._
import EvalTypes._
import scala.concurrent._
import duration._
import DisjunctionCreation._

/**
 * Effect for Future computations
 */
trait FutureEffect extends
  FutureCreation with
  FutureInterpretation

object FutureEffect extends FutureEffect

trait FutureCreation {

  type _Future[R] = Future <= R
  type _future[R] = Future |= R

  def sync[R :_future, A](a: A): Eff[R, A] =
    pure(a)

  def async[R :_future, A](a: =>A)(implicit ec: ExecutionContext): Eff[R, A] =
    send(Future(a))

  def liftFuture[R :_future :_eval, A](f: =>Future[A]): Eff[R, A] =
    EvalEffect.delay(f).flatMap(v => Eff.send[Future, R, A](v))

  def attemptFuture[R :_future :_eval :_throwableOr, A](f: =>Future[A])(implicit ec: ExecutionContext): Eff[R, A] =
    liftFuture(f.attempt).flatMap(send(_))

}

trait FutureInterpretation {

  def awaitFuture[R, U, A](r: Eff[R, A])(atMost: FiniteDuration)
      (implicit m: Member.Aux[Future, R, U], ec: ExecutionContext): Eff[U, Throwable \/ A] = {
    val recurse = new Recurse[Future, U, Throwable \/ A] {
      def apply[X](m: Future[X]) =
        try { -\/(Await.result(m, atMost)) }
        catch { case NonFatal(t) => \/-(Eff.pure(-\/(t))) }
    }

    interpret1((a: A) => \/-(a): Throwable \/ A)(recurse)(r)
  }

}

object FutureInterpretation extends FutureInterpretation

