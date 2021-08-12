package org.specs2

import scala.concurrent.duration.*
import runner.*
import execute.*
import concurrent.*
import matcher.*
import specification.core.*

class TimeoutSpec(env: Env) extends Specification with ResultMatchers:
  def is = section("ci") ^ s2"""

  a timeout can be set on an execution $executionTimeout
  a timeout can be set on a specification to timeout its examples $timeout

  """

  def executionTimeout =
    val execution = Execution.result { Thread.sleep(1000); ok }.setTimeout(100.millis)
    execution.startExecution(env).executionResult.runOption(env.executionEnv) must beSome(beFailing[Result])

  def timeout =
    val messages = TextRunner.run(TimeoutSpecExample)(env).messages
    messages must contain(
      "[error]  timeout after 100 milliseconds (TimeoutSpec.scala:31)"
    )

object TimeoutSpecExample extends Specification:
  def is = args(timeout = 100.millis) ^ s2"""

  timeout this example $tooLong

  """

  def tooLong: Result =
    Thread.sleep(500)
    ok
