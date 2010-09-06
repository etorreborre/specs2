package org.specs2.specification

trait ExampleExecution {
  def execute(body: () => Result): Result = body()
}