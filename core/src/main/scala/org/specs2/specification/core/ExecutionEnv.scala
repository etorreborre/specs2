package org.specs2
package specification
package core

import control._
import scala.concurrent.duration.Duration
import java.util.concurrent._

case class ExecutionEnv(timeOut:  Option[Duration] = None,
                        withoutIsolation: Boolean  = false,
                        timer:    Timer            = new Timer) {

  /**
   * fragments must not be created as "isolated"
   */
  def setWithoutIsolation = copy(withoutIsolation = true)

  lazy val executor = {
    timer.start
    ExecutionEnv.defaultExecutor
  }

  def shutdown = {
    try     executor.shutdown()
    finally timer.stop
  }
}

object ExecutionEnv {
  def defaultExecutor = executor(Runtime.getRuntime.availableProcessors)
  def executor(threadsNb: Int) = Executors.newFixedThreadPool(threadsNb, new NamedThreadFactory("specs2.DefaultExecutionStrategy"))
}


