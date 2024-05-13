package org.specs2
package concurrent

import org.specs2.control.Logger
import org.specs2.main.Arguments

import scala.concurrent.ExecutionContext

/**
 * Execution environment for native
 */
case class ExecutionEnv(executorServices: ExecutorServices,
                        timeFactor: Int,
                        retriesFactor: Int) {

  def shutdown(): Unit = ()

  def setTimeFactor(tf: Int): ExecutionEnv =
    copy(timeFactor = tf)

  def setRetriesFactor(tf: Int): ExecutionEnv =
    copy(retriesFactor = tf)

  lazy val executionContext = executorServices.executionContext
  lazy val scheduler = executorServices.scheduler

  implicit lazy val ec: ExecutionContext = executorServices.executionContext
}

object ExecutionEnv {

  /** create an ExecutionEnv from an execution context only */
  def fromExecutionContext(ec: =>ExecutionContext): ExecutionEnv =
    ExecutionEnv(
      ExecutorServices.fromExecutionContext(ec),
      timeFactor = 1,
      retriesFactor = 1)

  def create(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    fromGlobalExecutionContext

  def createSpecs2(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    fromGlobalExecutionContext

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutionEnv =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

}
