package org.specs2
package concurrent

import scala.concurrent.*, duration.*
import scala.util.*

/** run as a Future and raise a timeout exception if necessary NOTE: this does not execute the finalizers!!!
  */
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

/** Run the action and return an exception if it fails Whatever happens run the finalizers
  */
def awaitAction[A](
    runNow: ExecutionEnv => Future[A],
    timeout: Option[FiniteDuration],
    finalizeWith: =>Unit,
    ee: ExecutionEnv
): Either[Throwable, A] =
  throw new Exception("awaitAction can not be implemented for JavaScript")
