package org.specs2
package guide

import org.specs2.execute.{Success, ResultExecution, AsResult}

object IoIntegration extends UserGuidePage { def is = "IoIntegration".title ^ s2"""

Any IO-like data type which can be executed as a `Future` can be integrated in specifications.
Here is such a data type:
```
import scala.concurrent.*

case class IO[T](run: ExecutionContext => Future[T])
```

You can integrate it to any specification by providing an instance of the `AsExecution` typeclass:
```
import org.specs2.execute.{AsResult}
import org.specs2.specification.core.{AsExecution, Execution}

object IO {

  // this converts an IO value into a specs2 Execution with the
  // withEnvAsync function which takes a Future
  given [R : AsResult]: AsExecution[IO[R]] with
    def execute(io: =>IO[R]): Execution =
      Execution.withEnvAsync(env => io.run(env.executionContext))

  // create a successful IO value (used in the example below)
  def successful[T](t: =>T): IO[T] =
    IO(_ => Future.successful(t))

}
```

You can then use your IO type in a normal specification:
```
class TestMutableSpec extends mutable.Specification {
  "e1" >> {
    IO.successful(ok)
  }
}
```
"""
}
