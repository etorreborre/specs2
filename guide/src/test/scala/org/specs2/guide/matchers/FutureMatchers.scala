package org.specs2
package guide
package matchers

import execute.ExecutionTimeFactor
import org.specs2.specification.Environment
import org.specs2.specification.core.Env

import scala.concurrent.ExecutionContext._
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

// only retries, timeout is 1.second
Future { Thread.sleep(100); 1 } must be_>(0).retryAwait(retries = 2)

// only timeout, retries = 0
Future { Thread.sleep(100); 1 } must be_>(0).awaitFor(100.millis)
}}

Another possibility is for you to obtain a `Future[MatchResult[T]]` (or any `Future[R]` where `R` has an `AsResult typeclass instance). In that case you can use `await` directly on the `Future` to get a `Result`${snippet{
Future(1 === 1).await
Future(1 === 1).await(retries = 2, timeout = 100.millis)
}}

#### Scalaz Futures

All of the above is applicable to `scalaz.concurrent.Future` by using the method `attempt` instead of `await`.

#### Time factor

Some actions can be a lot slower when executed on a continuous integration server rather than a developer machine and some timeouts will fail.
You can avoid this by setting the `timeFactor` argument which will multiply the durations used when `awaiting / attempting` by a constant factor.

```
sbt> test-only *MyFuturesSpec* -- timeFactor 3
```

***Note***: if you are using the global execution context `scala.concurrent.ExecutionContext.Implicits.global` you need to "decorate" it first with the `timeFactor` value: ${snippet{
class MyFuturesSpec extends Specification with Environment { def is(env: Env) = {
  implicit val context = env.setTimeFactor(Implicits.global)
  s2"""
    check future ${ Future(1) must be_==(1).await }
  """
}}
}}


"""
}