package org.specs2
package guide

import java.util.concurrent.{ScheduledExecutorService, ExecutorService}

import execute._
import scala.concurrent._
import duration._

object ExecutionEnvironment extends UserGuidePage { def is = "Execution environment".title ^ s2"""

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

You can also use $specs2 execution environment directly (from now on code examples are provided for immutable specifications only but are transposable to mutable ones): ${snippet{
class MyFutureSpec extends Specification { def is = s2"""
 Let's check this scala future ${ implicit ee: ExecutionEnv =>
   Await.result(Future(1), Duration.Inf) must_== 1
 }
"""
}
}}

This works thanks to an implicit conversion between `ExecutionEnv` and `ExecutionContext` provided by the `org.specs2.execute.ImplicitExecutionContextFromExecutionEnv` trait (this can be deactivated by mixing-in the `NoImplicitExecutionContextFromExecutionEnv` trait). It is actually better to use an `ExecutionEnv` anyway because it is required when you want to ${"create `Future` matchers" ~/ Matchers} (see the "Future" tab).

### Scalaz Future

A Scalaz `Future` needs an implicit `ExecutorService` if you want to evaluate values asynchronously. You can require an `ExecutorService` in your examples like this: ${snippet{
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


"""
}
