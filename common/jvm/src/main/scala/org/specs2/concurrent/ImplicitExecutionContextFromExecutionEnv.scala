package org.specs2.concurrent

import scala.concurrent.ExecutionContext
import scala.implicits.Not

trait ImplicitExecutionContextFromExecutionEnv:
  /**
   * if an implicit execution environment is in scope, it can be used as an execution context
   */
  given executionEnvToExecutionContext(using ee: ExecutionEnv, n: Not[DeactivateImplicitExecutionContext]) as ExecutionContext =
    ee.executionContext


/**
 * deactivate the conversion between an implicit execution environment to an execution context
 */
trait DeactivateImplicitExecutionContext

trait NoImplicitExecutionContextFromExecutionEnv extends ImplicitExecutionContextFromExecutionEnv:
  given d as DeactivateImplicitExecutionContext =
    new DeactivateImplicitExecutionContext {}
