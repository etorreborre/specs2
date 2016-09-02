package org.specs2.control.eff.syntax

import org.specs2.control.eff._
import scala.concurrent._, duration._
import scalaz._
import FutureEffect._
import EvalEffect._
import DisjunctionEffect._

object future extends future

trait future {

  implicit class FutureEffectOps[R, A](e: Eff[R, A]) {

    def awaitFuture[U](atMost: FiniteDuration)
      (implicit member: Member.Aux[Future, R, U], ec: ExecutionContext): Eff[U, Throwable \/ A] =
      FutureInterpretation.awaitFuture(e)(atMost)

  }

  implicit class FutureOps[A](f: =>Future[A]) {

    def liftFuture[R :_future :_eval] =
      FutureEffect.liftFuture(f)

    def attemptFuture[R :_future :_eval :_throwableOr](implicit ec: ExecutionContext) =
      FutureEffect.attemptFuture(f)
  }

}
