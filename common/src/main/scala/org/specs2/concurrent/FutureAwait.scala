package org.specs2
package concurrent

import java.util.concurrent.TimeoutException
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

/**
 * implicit methods to await a future values with a given timeout and
 * number of retries
 */
trait FutureAwait {
  implicit class AwaitFuture[T](f: => Future[T])(implicit ee: ExecutionEnv) {
    def await: TimeoutFailure Either T =
      await(retries = 0, timeout = 1.second)

    def retry(retries: Int): TimeoutFailure Either T =
      await(retries, timeout = 1.second)

    def awaitFor(timeout: FiniteDuration): TimeoutFailure Either T =
      await(retries = 0, timeout)

    def await(retries: Int, timeout: FiniteDuration): TimeoutFailure Either T = {
      val tf = ee.timeFactor
      val appliedTimeout = timeout * tf.toLong

      def awaitFuture(remainingRetries: Int, totalDuration: FiniteDuration): TimeoutFailure Either T = {
        try Right(Await.result(f, appliedTimeout))
        catch {
          case e if e.getClass == classOf[TimeoutException] =>
            if (remainingRetries <= 0) Left(TimeoutFailure(appliedTimeout, totalDuration, tf))
            else                       awaitFuture(remainingRetries - 1, totalDuration + appliedTimeout)

          case other: Throwable  => throw other
        }
      }
      awaitFuture(retries, 0.second)
    }
  }

}

case class TimeoutFailure(appliedTimeout: FiniteDuration, totalDuration: FiniteDuration, timeFactor: Int)

object FutureAwait extends FutureAwait
