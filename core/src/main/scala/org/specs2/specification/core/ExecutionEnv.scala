package org.specs2
package specification
package core

import main.Arguments
import scala.collection.JavaConversions._
import control._
import scala.concurrent.duration.Duration
import java.util.concurrent._

case class ExecutionEnv(timeOut:  Option[Duration] = None,
                        withoutIsolation: Boolean  = false) {

  /**
   * fragments must not be created as "isolated"
   */
  def setWithoutIsolation =
    copy(withoutIsolation = true)
}

object ExecutionEnv {
  def defaultExecutor = executor(Runtime.getRuntime.availableProcessors)
  def executor(threadsNb: Int) = Executors.newFixedThreadPool(threadsNb, new NamedThreadFactory("specs2.DefaultExecutionStrategy"))
}


