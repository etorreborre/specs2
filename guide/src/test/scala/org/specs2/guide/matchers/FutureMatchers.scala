package org.specs2.guide
package matchers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

object FutureMatchers extends UserGuideCard {
  def title = "Future"
  def text = s2"""
Testing `Futures` is quite easy with $specs2. You can transform any `Matcher[T]` into a `Matcher[Future[T]` with the `await` method ${snippet{
  Future(1) must be_>(0).await
}}

You can also specify a timeout value and a number of retries ${snippet{
  Future { Thread.sleep(100); 1 } must be_>(0).await(retries = 2, timeout = 100.millis)
}}

Another possibility is for you to obtain a `Future[MatchResult[T]]` (or any `Future[R]` where `R` has an `AsResult typeclass instance). In that case you can use `await` directly on the `Future` to get a `Result`${snippet{
Future(1 === 1).await
Future(1 === 1).await(retries = 2, timeout = 100.millis)
}}

#### Scalaz Futures

All of the above is applicable to `scalaz.concurrent.Future` by using the method `attempt` instead of `await`.

"""
}
