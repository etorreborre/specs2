package org.specs2
package guide

import java.util.concurrent.ExecutorService
import org.specs2.matcher.TerminationMatchers
import specification.ExecutionEnvironment
import execute._
import concurrent.ExecutionEnv
import scala.concurrent._
import duration._

object ExecutionEnvironments extends UserGuidePage with TerminationMatchers { def is = "Execution environment".title ^ s2"""

When you run a specification, a `java.util.concurrent.ExecutorService` is used to execute examples concurrently. You can access this `ExecutorService` to execute `scalaz.concurrent.Futures` and it can also be wrapped into a `scala.concurrent.ExecutionContext` to create `scala.concurrent.Futures`.

### Scala Future

A Scala `Future` needs an implicit `ExecutionContext` to be created. You can reuse $specs2 own execution context if you want (instead of Scala's global one: `scala.concurrent.ExecutionContext.Implicits.global`): ${snippet{
class MyFutureSpec extends Specification { def is = s2"""
 Let's check this scala future ${ implicit ec: ExecutionContext =>
    Await.result(Future(1), Duration.Inf) must_== 1
 }
"""
}

// in a mutable specification
class MyMutableFutureSpec extends mutable.Specification {
  "Let's check this scala future" >> { implicit ec: ExecutionContext =>
    Await.result(Future(1), Duration.Inf) must_== 1
  }
}
}}

You can also use $specs2's execution environment directly (from now on code examples are provided for immutable specifications only but are transposable to mutable ones): ${snippet{
class MyFutureSpec extends Specification { def is = s2"""
 Let's check this scala future ${ implicit ee: ExecutionEnv =>
   Await.result(Future(1), Duration.Inf) must_== 1
 }
"""
}
}}

This works thanks to an implicit conversion between `ExecutionEnv` and `ExecutionContext` provided by the `org.specs2.execute.ImplicitExecutionContextFromExecutionEnv` trait (this can be deactivated by mixing-in the `NoImplicitExecutionContextFromExecutionEnv` trait). It is actually better to use an `ExecutionEnv` anyway because it is required when you want to ${"create `Future` matchers" ~/ Matchers} (see the "Future" tab).

### Scalaz Future

A Scalaz `Future` needs an implicit `ExecutorService` to evaluate values asynchronously. You can require an `ExecutorService` in your examples like this: ${snippet{
class MyFutureSpec extends Specification { def is = s2"""
 Let's check this scalaz future ${ implicit es: ExecutorService =>
   scalaz.concurrent.Future(1).run must_== 1
 }
"""
}
}}

And, similarly to the section above, you can get an implicit `ExecutorService` from an `ExecutionEnv`:${snippet{
class MyFutureSpec extends Specification { def is = s2"""
  Let's check this scalaz future ${ implicit ee: ExecutionEnv =>
    scalaz.concurrent.Future(1).run must_== 1
  }
"""
}
}}

You will also need a `ScheduledExecutorService` if you want to evaluate a Scalaz Future using the `timed` method (from Scalaz > 7.1): ${snippet {
class MyFutureSpec extends Specification { def is = s2"""
  Let's check this scalaz future ${ implicit ee: ExecutionEnv =>
    implicit val ses = ee.scheduledExecutorService
    scalaz.concurrent.Future(1).timed(3.seconds).run.toOption must beSome(1)
  }
"""
}
}}

### With matchers

Future $Matchers (see the "Future" tab) require an implicit `ExecutionEnv`. This environment is used to access:

 - the `timeFactor` when awaiting for Scala Futures
 - a `scheduledExecutorService` and the `timeFactor when attempting Scalaz Futures

The `terminate` matcher (see the "Termination" tab in the optional $Matchers section) also needs an `ExecutionEnv` to run a piece of code and periodically check if it has terminated or not: ${snippet{
  s2"""
  this code must be fast enough ${ implicit ee: ExecutionEnv =>
    Thread.sleep(100) must terminate(retries = 1, sleep = 60.millis)
  }
"""
}}

### Implicit ExecutionEnv

Passing an implicit `ExecutionEnv` for each example can be tedious. Another possibility is to mix-in the `org.specs2.specification.ExecutionEnvironment` trait to your specification: ${snippet{
class MyFutureSpec extends Specification with ExecutionEnvironment { def is(implicit ee: ExecutionEnv) = s2"""
 Let's check this scala future ${
   Future(1) must be_==(1).await
 }
"""
}

// in a mutable specification
class MyMutableFutureSpec extends mutable.Specification with specification.mutable.ExecutionEnvironment { def is(implicit ee: ExecutionEnv) = {
  "Let's check this scala future" >> {
    Future(1) must be_==(1).await
  }
}}
}}

$AndIfYouWantToKnowMore

 - use $specs2 ${"environment" ~/ Environment } in a Specification

$vid


"""
}
