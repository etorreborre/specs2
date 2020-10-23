package org
package specs2
package specification.core

import execute._
import scala.concurrent.Future

trait AsExecution[T]:
  def execute(t: =>T): Execution

object AsExecution:

  def apply[T](using t: AsExecution[T]): AsExecution[T] =
    t

  given [R : AsResult] as AsExecution[R]:
    def execute(r: => R): Execution =
      Execution.result(AsResult(r))

  given [R : AsResult] as AsExecution[Future[R]]:
    def execute(r: =>Future[R]): Execution =
      Execution.withEnvAsync(_ => r)
