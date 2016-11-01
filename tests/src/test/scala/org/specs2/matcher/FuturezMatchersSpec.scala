package org.specs2
package matcher

import org.specs2.concurrent.ExecutionEnv

import scala.concurrent.duration._
import scalaz.concurrent._
import FuturezMatchers._

class FuturezMatchersSpec(implicit ee: ExecutionEnv) extends Specification with ResultMatchers { def is = s2""" ${section("travis")}

 In this specification `Future` means `scalaz.concurrent.Future`

 Any `Matcher[T]` can be transformed into a `Matcher[Future[T]]` with the `attempt` method
 ${ Future.delay(1) must be_>(0).attempt }

 with a retries number and timeout
 ${ Future.delay { Thread.sleep(sleep); 1 } must be_>(0).attempt(retries = 3, timeout = 100.millis) }
 ${ Future.delay { Thread.sleep(sleep * 3); 1} must be_>(0).attempt(retries = 3, timeout = 10.millis) returns "Timeout" }

 with a retries number only
 ${ Future.delay { Thread.sleep(sleep); 1 } must be_>(0).retryAttempt(retries = 2) }

 with a timeout only
 ${ Future.delay { Thread.sleep(sleep); 1 } must be_>(0).attemptFor(200.millis) }


 A `Future` returning a `Matcher[T]` can be transformed into a `Result`
 ${ Future.delay(1 === 1).attempt }

 A `throwA[T]` matcher can be used to match a failed future with the `attempt` method
 ${ Future.delay { throw new RuntimeException; 1 } must throwA[RuntimeException].attempt }
 ${ { Future.delay{ throw new RuntimeException; 1 } must be_===(1).attempt } must throwA[RuntimeException] }

"""

  val timeFactor = ee.timeFactor
  val sleep = 50 * timeFactor.toLong
}
