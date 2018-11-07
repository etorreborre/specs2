package org.specs2
package guide
package matchers

import org.specs2.concurrent.ExecutionEnv
import specification.core.Env
import scala.concurrent._
import scala.concurrent.duration._

object FutureMatchers extends UserGuideCard {
  implicit lazy val ee = Env().executionEnv
  implicit lazy val ec = ee.ec

  def title = "Future"
  def text = s2"""
Testing `Futures` is quite easy with $specs2. You can simply return a value that is `Future[R]` where `R` has a `AsResult`
instance (meaning that `R` is some kind of result like: `Boolean`, `Result`, `MatchResult`,...).
Then your future will be executed when $specs2 executes your example and the result will be collected.

However you will not get the possibility to specify retries or timeouts. For retries and timeouts
you can use the `await` method on matchers: ${snippet{
Future(1) must be_>(0).await
}}

You can specify a timeout value and a number of retries ${snippet{
Future { Thread.sleep(100); 1 } must be_>(0).await(retries = 2, timeout = 100.millis)

// only retries, timeout is 1.second
Future { Thread.sleep(100); 1 } must be_>(0).retryAwait(retries = 2)

// only timeout, retries = 0
Future { Thread.sleep(100); 1 } must be_>(0).awaitFor(100.millis)
}}

Another possibility is for you to obtain a `Future[MatchResult[T]]` (or any `Future[R]` where `R` has an `AsResult` typeclass instance).
In that case you can use `await` directly on the `Future` to get a `Result`${snippet{
Future(1 === 1).await
Future(1 === 1).await(retries = 2, timeout = 100.millis)
}}

#### Scalaz Futures

All of the above is applicable to `scalaz.concurrent.Future` by using the method `attempt` instead of `await`.

#### Execution

The `await`/`attempt` methods require an implicit `org.specs2.concurrent.ExecutionEnv` (see [here](org.specs2.guide.ExecutionEnvironments.html) for more details). You can pass one in the body of your examples:${snippet{
import org.specs2.matcher.FuturezMatchers._

class MyFutureSpec(implicit ee: ExecutionEnv) extends Specification { def is = s2"""

 Let's check this scala future ${
   Future(1) must be_>(0).await
 }

 Let's check this scalaz future ${
   scalaz.concurrent.Future(1) must be_>(0).attempt
 }

"""
}

// in a mutable specification
class MyMutableFutureSpec(implicit ee: ExecutionEnv) extends mutable.Specification {

  "Let's check this scala future" >> {
    Future(1) must be_>(0).await
  }

  "Let's check this scalaz future" >> {
    scalaz.concurrent.Future(1) must be_>(0).attempt
  }

}
}}

#### Time factor

Some actions can be a lot slower when executed on a continuous integration server rather than a developer machine and some timeouts will fail.
You can avoid this by setting the `timeFactor` argument which will multiply the durations used when `awaiting / attempting` by a constant factor.

```
sbt> testOnly *MyFuturesSpec* -- timeFactor 3
```

$NowLearnTo

 - use the [execution environment](org.specs2.guide.ExecutionEnvironment.html)

$vid

"""
}
