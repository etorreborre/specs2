package org.specs2.concurrent

import scala.concurrent.ExecutionContext
import scala.util.Not

trait ImplicitExecutionContextFromExecutionEnv {
  /**
   * if an implicit execution environment is in scope, it can be used as an execution context
   */
  given executionEnvToExecutionContext(using ee: ExecutionEnv, not: Not[NoImplicitExecutionContextFromExecutionEnv]) as ee.executionContext
}

/**
 * deactivate the conversion between an implicit execution environment to an execution context
 */
trait NoImplicitExecutionContextFromExecutionEnv extends ImplicitExecutionContextFromExecutionEnv {
  given NoImplicitExecutionContextFromExecutionEnv = ???
}
