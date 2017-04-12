package org.specs2
package concurrent

import java.util.concurrent.ExecutorService
import scala.concurrent.ExecutionContext

/**
 * add implicit conversions between the execution environment and execution context / executor service
 */
trait ImplicitExecutionContexts extends
       ImplicitExecutionContextFromExecutionEnv
  with ImplicitExecutorServiceFromExecutionEnv

trait ImplicitExecutionContextFromExecutionEnv {
  /**
   * if an implicit execution environment is in scope, it can be used as an execution context
   */
  implicit def executionEnvToExecutionContext(implicit ee: ExecutionEnv): ExecutionContext =
    ee.executionContext
}

/**
 * deactivate the conversion between an implicit execution environment to an execution context
 */
trait NoImplicitExecutionContextFromExecutionEnv extends ImplicitExecutionContextFromExecutionEnv {
  override def executionEnvToExecutionContext(implicit ee: ExecutionEnv): ExecutionContext =
    super.executionEnvToExecutionContext(ee)
}

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
