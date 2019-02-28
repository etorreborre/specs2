package org.specs2.concurrent

import java.util.concurrent._

trait ImplicitExecutorServiceFromExecutionEnv {
  /**
   * if an implicit execution environment is in scope, it can be used as an executor service
   */
  implicit def executionEnvToExecutorService(implicit ee: ExecutionEnv): ExecutorService =
    ee.executorService
}

/**
 * deactivate the conversion between an implicit execution environment to an executor service
 */
trait NoImplicitExecutorServiceFromExecutionEnv extends ImplicitExecutorServiceFromExecutionEnv {
  override def executionEnvToExecutorService(implicit ee: ExecutionEnv): ExecutorService =
    super.executionEnvToExecutorService(ee)
}

