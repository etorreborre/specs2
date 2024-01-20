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

  def env: Env

  lazy val ownEnv: Env =
    env.copy(
      executionEnv = ExecutionEnv.create(env.arguments, env.systemLogger, tag = Some(getClass.getName)),
      specs2ExecutionEnv = ExecutionEnv.createSpecs2(env.arguments, env.systemLogger, tag = Some(getClass.getName))
    )

  given ee: ExecutionEnv =
    ownEnv.executionEnv

  given es: ExecutorServices =
    ownEnv.executorServices

  given ec: ExecutionContext =
    ownEnv.executionContext

  def afterSpec: Fragments =
    step(ownEnv.shutdown())
