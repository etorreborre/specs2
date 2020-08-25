package org.specs2.specification.core

import org.specs2.execute.AsResult

import scala.concurrent.Future

trait AsExecution[T]:
  def execute(t: =>T): Execution

object AsExecution:

  def apply[T](implicit t: AsExecution[T]): AsExecution[T] =
    t

  implicit def resultAsExecution[R : AsResult]: AsExecution[R] = new AsExecution[R] {
    def execute(r: => R): Execution = Execution.result(AsResult(r))
  }

  implicit def futureAsExecution[R : AsResult]: AsExecution[Future[R]] = new AsExecution[Future[R]] {
    def execute(r: =>Future[R]): Execution = Execution.withEnvAsync(_ => r)
  }
