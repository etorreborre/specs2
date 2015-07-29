package org.specs2
package matcher

import org.specs2.concurrent.ExecutionEnv
import specification.Environment
import specification.core.Env
import scala.concurrent.duration._
import scalaz.concurrent._

class FuturezMatchersSpec extends Specification with ResultMatchers with Environment { def is (env: Env) = {
val timeFactor = env.arguments.execute.timeFactor
val sleep = 50 * timeFactor

section("travis") ^
s2"""

 In this specification `Future` means `scalaz.concurrent.Future`

 Any `Matcher[T]` can be transformed into a `Matcher[Future[T]]` with the `attempt` method
 ${ implicit ee: EE => Future.delay(1) must be_>(0).attempt }

 with a retries number and timeout
 ${ implicit ee: EE => Future.delay { Thread.sleep(sleep); 1 } must be_>(0).attempt(retries = 3, timeout = 100.millis) }
 ${ implicit ee: EE => Future.delay { Thread.sleep(sleep * 3); 1} must be_>(0).attempt(retries = 3, timeout = 10.millis) returns "Timeout" }

 with a retries number only
 ${ implicit ee: EE => Future.delay { Thread.sleep(sleep); 1 } must be_>(0).retryAttempt(retries = 2) }

 with a timeout only
 ${ implicit ee: EE => Future.delay { Thread.sleep(sleep); 1 } must be_>(0).attemptFor(200.millis) }


 A `Future` returning a `Matcher[T]` can be transformed into a `Result`
 ${ implicit ee: EE => Future.delay(1 === 1).attempt }

 A `throwA[T]` matcher can be used to match a failed future with the `attempt` method
 ${ implicit ee: EE => Future.delay { throw new RuntimeException; 1 } must throwA[RuntimeException].attempt }
 ${ implicit ee: EE => { Future.delay{ throw new RuntimeException; 1 } must be_===(1).attempt } must throwA[RuntimeException] }

"""
}

  type EE = ExecutionEnv
}
