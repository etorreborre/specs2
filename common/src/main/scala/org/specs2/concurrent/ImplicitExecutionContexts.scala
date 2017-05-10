package org.specs2
package concurrent

import scala.concurrent.ExecutionContext

/**
 * add implicit conversions between the execution environment and execution context / executor service
 */
trait ImplicitExecutionContexts extends
       ImplicitExecutionContextFromExecutionEnv

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
