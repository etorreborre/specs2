package org.specs2
package specification
package core

import main.Arguments
import scala.collection.JavaConversions._
import control._
import scala.concurrent.duration.Duration
import java.util.concurrent._

case class ExecutionEnv(arguments: Arguments,
                        timeOut:  Option[Duration] = None,
                        withoutIsolation: Boolean  = false,
                        timer:    Timer            = new Timer) {

  /**
   * fragments must not be created as "isolated"
   */
  def setWithoutIsolation = {
    shutdown
    copy(withoutIsolation = true)
  }

  lazy val executor = {
    timer.start
    ExecutionEnv.executor(arguments.threadsNb)
  }

  def shutdown: Unit = {
    try     executor.shutdownNow
    finally timer.stop
  }
}

object ExecutionEnv {
  def defaultExecutor = executor(Runtime.getRuntime.availableProcessors)
  def executor(threadsNb: Int) = Executors.newFixedThreadPool(threadsNb, new NamedThreadFactory("specs2.DefaultExecutionStrategy"))
}


