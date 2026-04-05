package org.specs2
package concurrent

import scala.concurrent.*, duration.*
import scala.util.*

def runActionToFuture[A](
    runNow: ExecutionEnv => Future[A],
    timeout: Option[FiniteDuration],
    ee: ExecutionEnv
): Future[A] =
  timeout match
    case None =>
      runNow(ee)
    case Some(to) =>
      given ExecutionContext = ee.executionContext
      val promise = Promise[A]
      ee.schedule({ promise.tryFailure(new TimeoutException("timeout after " + to)); () }, to)
      promise.completeWith(runNow(ee))
      promise.future

def awaitAction[A](
    runNow: ExecutionEnv => Future[A],
    timeout: Option[FiniteDuration],
    finalizeWith: =>Unit,
    ee: ExecutionEnv
): Either[Throwable, A] =
  throw new Exception("awaitAction can not be implemented for Scala Native")
