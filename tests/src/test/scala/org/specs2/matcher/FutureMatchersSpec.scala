package org.specs2
package matcher

import org.specs2.specification.Environment
import org.specs2.specification.core.Env

import concurrent._
import duration._

class FutureMatchersSpec extends Specification with ResultMatchers with Retries with Environment { def is(env: Env) = {
 val timeFactor = env.arguments.execute.timeFactor
 val sleep = 100 * timeFactor
 s2"""

 In this specification `Future` means `scala.concurrent.Future`

 Any `Matcher[T]` can be transformed into a `Matcher[Future[T]]` with the `await` method
 ${ implicit ec: EC => Future.apply(1) must be_>(0).await }

 with a retries number and timeout
 ${ implicit ec: EC => Future { Thread.sleep(sleep); 1 } must be_>(0).await(retries = 3, timeout = 100.millis) }
 ${ implicit ec: EC => (Future { Thread.sleep(sleep); 1 } must be_>(0).await(retries = 4, timeout = 10.millis)) returns "Timeout" }

 with a retries number only
 ${ implicit ec: EC => Future { Thread.sleep(sleep); 1 } must be_>(0).retryAwait(2) }

 with a timeout only
 ${ implicit ec: EC => Future { Thread.sleep(sleep); 1 } must be_>(0).awaitFor(200.millis) }

 A `Future` returning a `Matcher[T]` can be transformed into a `Result`
 ${ implicit ec: EC => Future(1 === 1).await }

 A `throwA[T]` matcher can be used to match a failed future with the `await` method
 ${ implicit ec: EC => Future.failed[Int](new RuntimeException) must throwA[RuntimeException].await }
 ${ implicit ec: EC => { Future.failed[Int](new RuntimeException) must be_===(1).await } must throwA[RuntimeException] }

"""
}

  type EC = ExecutionContext
}