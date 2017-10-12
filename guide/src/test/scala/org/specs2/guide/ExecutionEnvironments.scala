package org.specs2
package guide

import java.util.concurrent.ExecutorService
import org.specs2.matcher.TerminationMatchers
import concurrent.ExecutionEnv
import scala.concurrent._
import duration._

object ExecutionEnvironments extends UserGuidePage with TerminationMatchers { def is = "Execution environment".title ^ s2"""

When you run a specification, a `java.util.concurrent.ExecutorService` is used to execute examples concurrently.
You can access this `ExecutorService` to execute `scalaz.concurrent.Futures` and it can also be wrapped into a
`scala.concurrent.ExecutionContext` to create `scala.concurrent.Futures`.

### Scala Future

A Scala `Future` needs an implicit `ExecutionContext` to be created. You can get an execution context shared across all
specifications by declaring it as a class member: ${snippet{
class MyFutureSpec(implicit ec: ExecutionContext) extends Specification { def is = s2"""
 Let's check this scala future ${
   Await.result(Future(1), Duration.Inf) must_== 1
 }
"""
}

// in a mutable specification
class MyMutableFutureSpec(implicit ec: ExecutionContext) extends mutable.Specification {
  "Let's check this scala future" >> {
    Await.result(Future(1), Duration.Inf) must_== 1
  }
}
}}

You can also use an `ExecutionEnv` (from now on code examples are provided for immutable specifications only but are transposable to mutable ones): ${snippet{
class MyFutureSpec(implicit ee: ExecutionEnv) extends Specification { def is = s2"""
 Let's check this scala future ${
   Await.result(Future(1), Duration.Inf) must_== 1
 }
"""
}
}}

This works thanks to an implicit conversion between `ExecutionEnv` and `ExecutionContext` provided by the
`org.specs2.execute.ImplicitExecutionContextFromExecutionEnv` trait
(this can be deactivated by mixing-in the `NoImplicitExecutionContextFromExecutionEnv` trait).
It is actually better to use an `ExecutionEnv` anyway because it is required when you want to ${"create `Future` matchers" ~/ Matchers} (see the "Future" tab).
Indeed an `ExecutionEnv` contains a `timeFactor` which can be used to modify the timeout from the command line and
wait longer for Futures executing on a continuous integration server for example.

### Scalaz Future

A Scalaz `Future` needs an implicit `ExecutorService` to evaluate values asynchronously.
You can require an `ExecutorService` in your examples like this: ${snippet{
class MyFutureSpec(implicit es: ExecutorService) extends Specification { def is = s2"""
 Let's check this scalaz future ${
   scalaz.concurrent.Future(1).run must_== 1
 }
"""
}
}}

And, similarly to the section above, you can get an implicit `ExecutorService` from an `ExecutionEnv`:${snippet{
class MyFutureSpec(implicit ee: ExecutionEnv) extends Specification { def is = s2"""
  Let's check this scalaz future ${
    scalaz.concurrent.Future(1).run must_== 1
  }
"""
}
}}

You will also need a `ScheduledExecutorService` if you want to evaluate a Scalaz Future using the `timed` method (from Scalaz > 7.1): ${snippet {
class MyFutureSpec(implicit ee: ExecutionEnv) extends Specification { def is = s2"""
  Let's check this scalaz future ${
    implicit val ses = ee.scheduledExecutorService
    scalaz.concurrent.Future(1).timed(3.seconds).run.toOption must beSome(1)
  }
"""
}
}}

### With matchers

Future $Matchers (see the "Future" tab) require an implicit `ExecutionEnv`. This environment is used to access:

 - the `timeFactor` when awaiting for Scala Futures
 - a `scheduledExecutorService` and the `timeFactor` when attempting Scalaz Futures

The `terminate` matcher (see the "Termination" tab in the optional $Matchers section) also needs an `ExecutionEnv` to run a piece of code and periodically check if it has terminated or not: ${snippet{
  s2"""
  this code must be fast enough ${
    implicit val ee = ExecutionEnv.fromGlobalExecutionContext
    Thread.sleep(100) must terminate(retries = 1, sleep = 60.millis)
  }
"""
}}

### One per specification

If you want to have exactly one `Env` or one `ExecutionEnv` per `Specification` you can mix-in the `org.specs2.specicication.core.OwnEnv`
or the `org.specs2.specicication.core.OwnExecutionEnv` traits. You will then get a specific threadpool instantiated and
shutdown just for the execution of one specification. See the ${"environment" ~/ Environment } page for more information.

$AndIfYouWantToKnowMore

 - use $specs2 ${"environment" ~/ Environment } in a Specification

$vid


"""
}
