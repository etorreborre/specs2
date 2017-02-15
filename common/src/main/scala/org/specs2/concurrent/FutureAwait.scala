package org.specs2
package concurrent

import java.util.concurrent.TimeoutException
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scalaz._, Scalaz._

/**
 * implicit methods to await a future values with a given timeout and
 * number of retries
 */
trait FutureAwait {
  implicit class AwaitFuture[T](f: => Future[T])(implicit ee: ExecutionEnv) {
    def await: TimeoutFailure \/ T =
      await(retries = 0, timeout = 1.second)

    def retry(retries: Int): TimeoutFailure \/ T =
      await(retries, timeout = 1.second)

    def awaitFor(timeout: FiniteDuration): TimeoutFailure \/ T =
      await(retries = 0, timeout)

    def await(retries: Int, timeout: FiniteDuration): TimeoutFailure \/ T = {
      val tf = ee.timeFactor
      val appliedTimeout = timeout * tf.toLong

      def awaitFuture(remainingRetries: Int, totalDuration: FiniteDuration): TimeoutFailure \/ T = {
        try Await.result(f, appliedTimeout).right
        catch {
          case e if e.getClass == classOf[TimeoutException] =>
            if (remainingRetries <= 0) TimeoutFailure(appliedTimeout, totalDuration, tf).left
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
