package org.specs2
package specification
package core

import scala.concurrent.ExecutionContext
import fp.*, syntax.*
import concurrent.{ExecutorServices, ExecutionEnv}
import execute.*, StandardResults.*
import specification.*
import dsl.*

/** This trait copies the inherited env: Env for a Specification and makes sure it is shutdown when the specification
  * finishes
  *
  * The specification using this trait must require an Env:
  *
  * class MySpec(env: Env) extends Specification with OwnEnv
  */
trait OwnEnv extends AfterSpec:
  self: FragmentsDsl =>

  // start an environment for this class
  lazy val env: Env =
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
    env.executionEnv

  given es: ExecutorServices =
    env.executorServices

  given ec: ExecutionContext =
    env.executionContext

  def afterSpec: Fragments =
    step(env.shutdown())
