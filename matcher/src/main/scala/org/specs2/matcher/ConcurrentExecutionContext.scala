package org.specs2.matcher

import scala.concurrent.ExecutionContext

/**
 * Specification of the execution context to be used for executing futures
 * This can be overridden to pass in your own execution context
 */
@deprecated(message = "The execution context needs to be user-defined but you can reuse specs2 execution context by defining an example as a function `ExecutionContext => AsResult[T]`", since = "3.0")
trait ConcurrentExecutionContext {
  implicit def concurrentExecutionContext: ExecutionContext = concurrent.ExecutionContext.Implicits.global
}

/**
 * stack this trait to remove the implicit execution context used to evaluate features
 */
@deprecated(message = "The execution context needs to be user-defined but you can reuse specs2 execution context by defining an example as a function `ExecutionContext => AsResult[T]`", since = "3.0")
trait NoConcurrentExecutionContext extends ConcurrentExecutionContext {
  override def concurrentExecutionContext: ExecutionContext = super.concurrentExecutionContext
}
