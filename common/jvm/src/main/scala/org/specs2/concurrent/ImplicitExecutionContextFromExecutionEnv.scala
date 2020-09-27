package org.specs2.concurrent

import scala.concurrent.ExecutionContext
import scala.implicits.Not

trait ImplicitExecutionContextFromExecutionEnv:
  /**
   * if an implicit execution environment is in scope, it can be used as an execution context
   */
  given executionEnvToExecutionContext(using ee: ExecutionEnv, not: Not[NoImplicitExecutionContextFromExecutionEnv]) as ExecutionContext =
    ee.executionContext


trait NoImplicitExecutionContextFromExecutionEnv extends ImplicitExecutionContextFromExecutionEnv:
  given NoImplicitExecutionContextFromExecutionEnv = ???
