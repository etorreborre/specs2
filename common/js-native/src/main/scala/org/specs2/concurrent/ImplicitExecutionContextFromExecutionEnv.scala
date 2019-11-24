package org.specs2.concurrent

import scala.concurrent.ExecutionContext

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
