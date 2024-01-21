package org.specs2
package specification
package core

import concurrent.ExecutionEnv
import dsl.*

import scala.concurrent.ExecutionContext

/** This trait provides an isolated Execution Env for a Specification and makes sure it is shutdown when the
  * specification finishes.
  *
  * The specification using this trait must require an Env:
  *
  * class MySpec(env: Env) extends Specification with OwnExecutionEnv
  */
trait OwnExecutionEnv extends AfterSpec:
  self: FragmentsDsl =>

  lazy val executionEnv: ExecutionEnv =
    env.executionEnv

  private lazy val env: Env =
    Env(
      EnvDefault.default.arguments,
      EnvDefault.default.resources,
      EnvDefault.default.systemLogger,
      EnvDefault.default.printerLogger,
      EnvDefault.default.statisticsRepository,
      EnvDefault.default.random,
      EnvDefault.default.fileSystem,
      EnvDefault.default.customClassLoader,
      EnvDefault.default.classLoading,
      ExecutionEnv.create(EnvDefault.default.arguments, EnvDefault.default.systemLogger, tag = Some(getClass.getName)),
      ExecutionEnv.createSpecs2(
        EnvDefault.default.arguments,
        EnvDefault.default.systemLogger,
        tag = Some(getClass.getName)
      )
    )

  given ee: ExecutionEnv =
    executionEnv

  lazy val ec: ExecutionContext =
    env.executionContext

  def afterSpec: Fragments =
    step(env.shutdown())
