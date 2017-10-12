package org.specs2
package guide

import scala.concurrent._, duration._
import specification.core.{Env, OwnEnv, OwnExecutionEnv}
import io._
import control._

object Environment extends UserGuidePage { def is = "Environment".title ^ s2"""

The execution of a Specification depends on various parts, among which:

 - the command line arguments
 - an `ExecutorService` for concurrent execution
 - a "StatisticsRepository" to access previous results
 - a "Logger" to log results to the console
 - an interface for the file system

<p/>All of this is bundled into one object `org.specs2.specification.core.Env`. The `Env` is accessible to your Specification by either:

 - having it injected as a Specification member
 - extending a trait

### Dependency injection

The following objects can be injected in your specification if you declare a 1-parameter constructor:

  - the `Env` itself
  - the `Arguments` object
  - the `CommandLine` object
  - the `ExecutionEnv` object (can be implicit)
  - the `ExecutionContext` object (can be implicit)

<p/>For example: ${snippet{
class MySpec(env: Env) extends Specification { def is = s2"""
  Use the environment fileSystem
  ${ env.fileSystem.mkdirs("tmp" / "test").runOption; ok }
"""
}
}}

Or if you want to access an `ExecutionContext`:${snippet{
class MySpec(implicit ec: ExecutionContext) extends Specification { def is = s2"""
  Use a future
  ${ Await.result(Future(1), 1.seconds) must_== 1 }
"""
  }
}}

### Own Env / ExecutionEnvironment

The `ExecutionEnv` which is injected in a specification will be shared with all specifications. If you want to provide
some isolation between your specifications and get a specific thread pool being dedicated to your specification you use
the `org.specs2.specification.core.OwnEnv` or `org.specs2.specification.core.OwnExecutionEnv` traits:${snippet{
class MySpec(val env: Env) extends Specification with OwnExecutionEnv { def is = s2"""
  Use a future
  ${ Await.result(Future(1), 1.seconds) must_== 1 }
"""
  }
}}

You need to inject a public `env` which will be duplicated to create an implicit `ExecutionEnv` for the sole use of your
specification (and shutdown when your specification has been executed). Doing so ensures that command line arguments
influencing the execution of your specification, like `threadsnb` or `timefactor` will be used.

$AndIfYouWantToKnowMore

 - use $specs2 ${"execution environments" ~/ ExecutionEnvironments} in a Specification

 $vid

"""

}
