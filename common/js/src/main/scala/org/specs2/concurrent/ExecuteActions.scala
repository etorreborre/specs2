package org.specs2
package concurrent

import scala.concurrent.{Future}
import scala.concurrent.duration.{FiniteDuration}

/**
 * run as a Future and raise a timeout exception if necessary
 * NOTE: this does not execute the finalizers!!!
 */
def runActionToFuture[A](runNow: ExecutionEnv => Future[A], timeout: Option[FiniteDuration], ee: ExecutionEnv): Future[A] =
  runNow(ee)

/**
 * Run the action and return an exception if it fails
 * Whatever happens run the finalizers
 */
def awaitAction[A](runNow: ExecutionEnv => Future[A], timeout: Option[FiniteDuration], finalizeWith: =>Unit, ee: ExecutionEnv): Throwable Either A =
  throw new Exception("awaitAction can not be implemented for JavaScript")
