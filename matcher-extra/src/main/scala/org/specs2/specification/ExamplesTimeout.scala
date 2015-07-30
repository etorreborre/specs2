package org.specs2
package specification

import execute._
import matcher._
import org.specs2.concurrent.ExecutionEnv
import specification.core._
import scala.concurrent.duration._
import TerminationMatchers._

/**
 * This trait can be used to add a global time out to each example or for a specific one:
 *
 *  - for each example mix-in the trait
 *  - for a single example import the object and use the upTo context:
 *
 *   my example must terminate in a reasonable amount of time \${upTo(3.seconds)(e1)}
 */
trait ExamplesTimeout extends EachContext with AroundTimeout {

  def context: Env => Context = { env: Env =>
    val timeout = env.arguments.commandLine.intOr("timeout", 1000 * 60).millis
    aroundTimeout(timeout)(env.executionEnv)
  }
}

object ExamplesTimeout extends ExamplesTimeout

trait AroundTimeout {

  def upTo[T](to: Duration)(t: => T)(implicit asResult: AsResult[T], ee: ExecutionEnv) =
    aroundTimeout(to)(ee).apply(t)

  def aroundTimeout(to: Duration)(implicit ee: ExecutionEnv): Around =
    new Around {
      def around[T : AsResult](t: =>T) = {
        lazy val result = t
        val termination = terminate(retries = 10, sleep = (to.toMillis / 10).millis).orSkip(_ => "TIMEOUT: "+to)(Expectable(result))

        if (!termination.toResult.isSkipped) AsResult(result)
        else termination.toResult
      }
    }
}

object AroundTimeout extends AroundTimeout

