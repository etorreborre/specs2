package org.specs2
package specification

import java.util.concurrent.ExecutorService

import execute._
import matcher._
import specification.core._
import time._

import scala.concurrent.duration._

/**
 * This trait can be used to add a global time out to each example or for a specific one:
 *
 *  - for each example mix-in the trait
 *  - for a single example import the object and use the upTo context:
 *
 *   my example must terminate in a reasonable amount of time ${upTo(3.seconds)(e1)}
 */
trait ExamplesTimeout extends EachContext with MustMatchers with TerminationMatchers {

  def context: Env => Context = { env: Env =>
    val timeout = env.arguments.commandLine.intOr("timeout", 1000 * 60).millis
    upTo(timeout)(env.executorService)
  }

  def upTo(to: Duration)(implicit es: ExecutorService) = new Around {
    def around[T : AsResult](t: =>T) = {
      lazy val result = t
      val termination = result must terminate(retries = 10, sleep = (to.toMillis / 10).millis).orSkip((ko: String) => "TIMEOUT: "+to)

      if (!termination.toResult.isSkipped) AsResult(result)
      else termination.toResult
    }
  }

}

object ExamplesTimeout extends ExamplesTimeout

