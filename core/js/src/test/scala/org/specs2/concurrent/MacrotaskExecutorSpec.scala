package org.specs2
package concurrent

import scala.concurrent.*, duration.*
import LoopingCode.*
import scala.scalajs.*
import org.scalajs.macrotaskexecutor.MacrotaskExecutor
import specification.core.*
import runner.*
import main.*

class MacrotaskExecutorSpec(env: Env) extends Specification:
  def is = s2"""

  An example can be timed-out when using ScalaJS $timeoutExample

  """

  def timeoutExample =
    given ExecutionContext = env.executionContext
    TextRunner.runFuture(MacrotaskExecutorSpecification())(env.setArguments(Arguments())).map { output =>
      output.messages must contain(contain("timeout after"))
    }

class MacrotaskExecutorSpecification extends Specification:
  def is = args.execute(timeout = 500.millis) ^ s2"""

  An example can be timed-out when using ScalaJS $timeoutExample

  """

  def timeoutExample =
    given ExecutionContext = MacrotaskExecutor
    loop.map(_ => ok)

object LoopingCode:
  var cancel = false

  def loop(using executionContext: ExecutionContext): Future[Unit] =
    Future(cancel).flatMap(canceled => if canceled then Future.unit else loop)
