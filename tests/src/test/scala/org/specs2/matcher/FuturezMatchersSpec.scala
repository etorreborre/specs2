package org.specs2
package matcher

import scala.concurrent.duration._
import scalaz.concurrent._
import java.util.concurrent.ExecutorService

class FuturezMatchersSpec extends Specification with ResultMatchers { def is = section("travis") ^ sequential ^ s2"""

 In this specification `Future` means `scalaz.concurrent.Future`

 Any `Matcher[T]` can be transformed into a `Matcher[Future[T]]` with the `attempt` method
 ${ implicit es: ES => Future.delay(1) must be_>(0).attempt }

 with a retries number
 ${ implicit es: ES => Future.delay { Thread.sleep(100); 1 } must be_>(0).attempt(retries = 2, timeout = 100.millis) }

 ${ implicit es: ES =>
   Future.delay { Thread.sleep(300); 1} must be_>(0).attempt(retries = 4, timeout = 10.millis) returns
   "Timeout after 50 milliseconds"
  }

 A `Future` returning a `Matcher[T]` can be transformed into a `Result`
 ${ implicit es: ES => Future.delay(1 === 1).attempt }

 A `throwA[T]` matcher can be used to match a failed future with the `attempt` method
 ${ implicit es: ES => Future.delay { throw new RuntimeException; 1 } must throwA[RuntimeException].attempt }
 ${ implicit es: ES => { Future.delay{ throw new RuntimeException; 1 } must be_===(1).attempt } must throwA[RuntimeException] }

"""

  type ES = ExecutorService
}
