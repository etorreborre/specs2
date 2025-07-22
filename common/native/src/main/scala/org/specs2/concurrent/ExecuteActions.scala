package org.specs2
package concurrent

import scala.concurrent.{Future, Promise, TimeoutException, Await}
import scala.concurrent.duration.{FiniteDuration, Duration}

/** run as a Future and raise a timeout exception if necessary NOTE: this does not execute the finalizers!!!
  */
def runActionToFuture[A](
    runNow: ExecutionEnv => Future[A],
    timeout: Option[FiniteDuration],
    ee: ExecutionEnv
): Future[A] =
  timeout.fold(runNow(ee)) { t =>
    val promise = Promise[A]
    val maxTime = t * ee.timeFactor.toLong
    ee.executorServices.schedule({ promise.tryFailure(new TimeoutException(s"timeout after $maxTime")); () }, maxTime)
    promise.completeWith(runNow(ee))
    promise.future
  }

/** Run the action and return an exception if it fails Whatever happens run the finalizers
  */
def awaitAction[A](
    runNow: ExecutionEnv => Future[A],
    timeout: Option[FiniteDuration],
    finalizeWith: =>Unit,
    ee: ExecutionEnv
): Throwable Either A =
  try Right(Await.result(runActionToFuture(runNow, timeout, ee), timeout.getOrElse(Duration.Inf)))
  catch { case t: Throwable => Left(t) }
  finally finalizeWith
