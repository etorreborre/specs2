package org.specs2
package specification

import execute.AsResult
import main.CommandLineArguments
import matcher.{Expectable, TerminationMatchers}
import time.{TimeConversions, Duration}
import TerminationMatchers._
import TimeConversions._

/**
 * This trait can be used to add a global time out to each example or for a specific one:
 *
 *  - for each example mix-in the trait
 *  - for a single example import the object and use the upTo context:
 *      my example must terminate in a reasonable amount of time ${upTo(3.seconds)(e1)}
 */
trait ExamplesTimeout extends AroundExample with CommandLineArguments {

  lazy val commandLineTimeOut = arguments.commandLine.int("timeout").map(_.millis)

  lazy val defaultTimeOut = 1.minute

  def timeout = commandLineTimeOut.getOrElse(defaultTimeOut)

  def around[T : AsResult](t: =>T) = upTo(timeout).around(t)

  def upTo(to: Duration) = new Around {

    def around[T : AsResult](t: =>T) = {
      lazy val result = t
      val termination = terminate(retries = 10, sleep = (to.inMillis / 10).millis).orSkip(_ => "TIMEOUT: "+to).apply(Expectable(result))

      if (!termination.toResult.isSkipped) AsResult(result)
      else termination.toResult
    }
  }

}

object ExamplesTimeout extends ExamplesTimeout
