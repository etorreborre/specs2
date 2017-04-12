package org.specs2.control.eff
package syntax

import org.specs2.concurrent.ExecutorServices
import scala.concurrent._

object future extends future

trait future {

  implicit final def toFutureOps[R, A](e: Eff[R, A]): FutureOps[R, A] = new FutureOps[R, A](e)

}

final class FutureOps[R, A](val e: Eff[R, A]) extends AnyVal {
  def futureAttempt(implicit future: TimedFuture /= R): Eff[R, Throwable Either A] =
    FutureInterpretation.futureAttempt(e)

  def futureMemo(key: AnyRef, cache: Cache)(implicit future: TimedFuture /= R): Eff[R, A] =
    FutureInterpretation.futureMemo(key, cache, e)

  def runAsync(implicit es: ExecutorServices, m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    FutureInterpretation.runAsync(e)

  def runSequential(implicit es: ExecutorServices, m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    FutureInterpretation.runSequential(e)
}



