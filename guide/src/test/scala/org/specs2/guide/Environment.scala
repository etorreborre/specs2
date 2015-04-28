package org.specs2
package guide

import main.CommandLine
import concurrent.ExecutionEnv
import scala.concurrent.{ExecutionContext, Future, duration, Await}, duration._
import specification._
import specification.core.Env
import io._
import control._

object Environment extends UserGuidePage { def is = "Environment".title ^ s2"""

The execution of a Specification depends on various parts, among which:

 - the command line arguments
 - an `ExecutorService` for concurrent execution
 - a "StatisticsRepository" to access previous results
 - a "Logger" to log results to the console
 - an interface for the file system

All of this is bundled into one object `org.specs2.specification.core.Env`. The `Env` is accessible to your Specification by either:

 - having it injected as a Specification member
 - extending a trait

### Dependency injection

The following objects can be injected in your specification if you declare a 1-parameter constructor:

  - the `Env` itself
  - the `Arguments` object
  - the `CommandLine` object
  - the `ExecutionEnv` object (can be implicit)
  - the `ExecutionContext` object
  - the `ExecutorService` object

For example: ${snippet{
class MySpec(env: Env) extends Specification { def is =
s2"""
  Use the environment fileSystem
  ${ env.fileSystem.mkdirs("tmp" / "test").runOption; ok }
"""
}
}}

Or if you want to access an `ExecutionContext`:${snippet{
class MySpec(implicit ec: ExecutionContext) extends Specification { def is =
s2"""
  Use a future
  ${ Await.result(Future(1), 1.seconds) must_== 1 }
"""
  }
}}

### Environment traits

The `Env` object can be access by mixing-in the `org.specs2.specification.Environment` trait ${snippet{
class MySpec extends Specification with Environment { def is(env: Env) =
s2"""
  Use the environment fileSystem
  ${ env.fileSystem.mkdirs("tmp" / "test").runOption; ok }
"""
}
}}

As you can see, instead of defining the `is` method you now need to defined the `is(env: Env)` method. Then you can access any attribute of the current `Env`.
There are also some specialised traits giving access to specific parts of the environment

#### Command-line arguments

When you just want to access the command-line arguments you can use the `org.specs2.specification.CommandLineArguments` trait${snippet {
class MySpec extends Specification with CommandLineArguments { def is(args: CommandLine) = s2"""
  Use the command line arguments
  ${ if (args.isSet("pass")) ok else ko }
"""
}
}}

#### Execution environment

When you just want to access the execution environment can use the `org.specs2.specification.ExecutionEnvironment` trait${snippet {
  class MySpec extends Specification with ExecutionEnvironment { def is(implicit ee: ExecutionEnv) = s2"""
  Use the implicit execution environment
  ${ Future(1) must be_==(1).await }
"""
  }
}}

As you can see the `ExecutionEnv` parameter is defined as an implicit parameter because this is what is required when creating futures or using ${"Future matchers" ~/ ExecutionEnvironments}

"""

}
