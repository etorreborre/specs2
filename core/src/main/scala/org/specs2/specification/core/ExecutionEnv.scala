package org.specs2
package specification
package core

import control._
import scala.concurrent.duration.Duration
import java.util.concurrent._

/**
 * Subset of the Env describing execution parameters
 *
 * if withoutIsolation is true then fragments are executed right away because they are
 * already in their own copy of the specification
 */
case class ExecutionEnv(timeOut:  Option[Duration] = None,
                        withoutIsolation: Boolean  = false) {

  /**
   * fragments must not be created as "isolated"
   */
  def setWithoutIsolation =
    copy(withoutIsolation = true)
}

object ExecutionEnv {

  def defaultExecutor =
    executor(Runtime.getRuntime.availableProcessors, "DefaultExecutionStrategy")

  /**
   * the number of executors is set from the arguments.threadsNb value which is
   * Runtime.getRuntime.availableProcessors by default
   */
  def executor(threadsNb: Int, name: String) =
    Executors.newFixedThreadPool(threadsNb, new NamedThreadFactory("specs2."+name))
}


