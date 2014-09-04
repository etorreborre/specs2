package org.specs2
package matcher

import specification._
import script._
import concurrent._
import duration._
import ExecutionContext.Implicits.global
import java.util.concurrent.{Executors, ThreadPoolExecutor, ForkJoinPool, Executor}

class FutureMatchersSpec extends Specification with Groups with ResultMatchers with Retries { def is = sequential ^ s2"""

 Any `Matcher[T]` can be transformed into a `Matcher[Future[T]]` with the `await` method
 ${ implicit ec: EC => Future.apply(1) must be_>(0).await }

 with a retries number
 ${ implicit ec: EC => Future { Thread.sleep(100); 1 } must be_>(0).await(retries = 2, timeout = 100.millis) }
 ${ implicit ec: EC => (Future { Thread.sleep(800); 1 } must be_>(0).await(retries = 4, timeout = 50.millis)) returns "Timeout after 250 milliseconds" }

 A `Future` returning a `Matcher[T]` can be transformed into a `Result`
 ${ implicit ec: EC => Future(1 === 1).await }

 A `throwA[T]` matcher can be used to match a failed future with the `await` method
 ${implicit ec: EC => Future.failed[Int](new RuntimeException) must throwA[RuntimeException].await }
 ${implicit ec: EC => { Future.failed[Int](new RuntimeException) must be_===(1).await } must throwA[RuntimeException] }

"""

  type EC = ExecutionContext
}
