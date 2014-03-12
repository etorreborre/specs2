package org.specs2
package matcher

import specification._
import script._
import org.specs2.time.{NoTimeConversions}
import concurrent._
import duration._
import ExecutionContext.Implicits.global

class FutureMatchersSpec extends Specification with Groups with NoTimeConversions with ResultMatchers { def is = s2"""

 Any `Matcher[T]` can be transformed into a `Matcher[Future[T]]` with the `await` method
 ${ Future(1) must be_>(0).await }

 with a retries number
 ${ Future { Thread.sleep(100); 1 } must be_>(0).await(retries = 2, timeout = 100.millis) }
 ${ (Future { Thread.sleep(800); 1 } must be_>(0).await(retries = 4, timeout = 50.millis)) returns "Timeout after 200 milliseconds" }

 A `Future` returning a `Matcher[T]` can be transformed into a `Result`
 ${ Future(1 === 1).await }

"""

  // the current execution context can be overridden here
  override implicit val concurrentExecutionContext: ExecutionContext = concurrent.ExecutionContext.Implicits.global

}
