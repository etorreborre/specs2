package org.specs2
package concurrent

import scala.concurrent.*, duration.*
import LoopingCode.*
import specification.core.*
import runner.*

class MacrotaskExecutorSpec(env: Env) extends Specification:
  def is = s2"""

  This specification is a copy of the coreJS/MacrotaskExecutorSpec to check
  if timeouts work ok on the JVM

  An example can be timed-out when using Scala on the JVM $timeoutExample

  """

  def timeoutExample =
    given ExecutionContext = env.executionContext
    TextRunner.runFuture(MacrotaskExecutorSpecification())(env).map { output =>
      output.messages must contain(contain("timeout after 500 milliseconds"))
    }

class MacrotaskExecutorSpecification extends Specification:
  def is = args.execute(timeout = 500.millis) ^ s2"""

  An example can be timed-out when using ScalaJS $timeoutExample

  """

  def timeoutExample =
    given ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
    loop.map(_ => ok)

object LoopingCode:
  var cancel = false

  def loop(using executionContext: ExecutionContext): Future[Unit] =
    Future(cancel).flatMap(canceled => if canceled then Future.unit else loop)
