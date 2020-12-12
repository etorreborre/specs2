package org.specs2
package specification
package core

import execute.StandardResults._
import concurrent.{ExecutorServices, ExecutionEnv}
import specification._
import scala.concurrent.ExecutionContext
import dsl._

/**
 * This trait copies the inherited env: Env for a Specification and makes sure it is shutdown
 * when the specification finishes
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
      executionEnv =       ExecutionEnv.create(env.arguments, env.systemLogger, tag = Some(getClass.getName)),
      specs2ExecutionEnv = ExecutionEnv.createSpecs2(env.arguments, env.systemLogger, tag = Some(getClass.getName)))

  given ee as ExecutionEnv =
    ownEnv.executionEnv

  lazy val es: ExecutorServices =
    ownEnv.executorServices

  lazy val ec: ExecutionContext =
    ownEnv.executionContext

  def afterSpec: Fragments =
    step(ownEnv.shutdown())
