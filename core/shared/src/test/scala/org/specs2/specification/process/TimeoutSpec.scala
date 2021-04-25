package org.specs2

import scala.concurrent.duration.*
import runner.*
import specification.core.*

class TimeoutSpec(env: Env) extends Specification:
  def is = s2"""

  a timeout can be set on a specification to timeout its examples $timeout

  """

  def timeout =
    val messages = TextRunner.run(TimeoutSpecExample)(env).messages
    messages must contain (allOf(
      "[info] TimeoutSpecExample",
      "[info] ",
      "[info]   + timeout this example",
      "[info] ",
      "[info] Total for specification TimeoutSpecExample",
      "[info] 1 example, 0 failure, 0 error"
    ))

object TimeoutSpecExample extends Specification:
  def is = args(timeout = 100.millis) ^ s2"""

  timeout this example $tooLong

  """

  def tooLong =
    (1 to 10000000).toList.reverse.sorted
    success
