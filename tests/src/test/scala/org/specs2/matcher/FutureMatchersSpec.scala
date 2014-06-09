package org.specs2
package matcher

import specification._
import core.Env
import script._
import concurrent._
import duration._

class FutureMatchersSpec extends Specification with Groups with ResultMatchers { def is = sequential ^ s2"""

 Any `Matcher[T]` can be transformed into a `Matcher[Future[T]]` with the `await` method
 ${ implicit c: ExecutionContext => Future(1) must be_>(0).await }

 with a retries number
 ${ implicit c: ExecutionContext => Future({ Thread.sleep(100); 1 }) must be_>(0).await(retries = 2, timeout = 100.millis) }
 ${ implicit c: ExecutionContext => (Future({ Thread.sleep(800); 1 }) must be_>(0).await(retries = 4, timeout = 50.millis)) returns "Timeout after 250 milliseconds" }

 A `Future` returning a `Matcher[T]` can be transformed into a `Result`
 ${ implicit c: ExecutionContext => Future(1 === 1).await }

"""

}

case class CustomException(e: String) extends Exception(e)
