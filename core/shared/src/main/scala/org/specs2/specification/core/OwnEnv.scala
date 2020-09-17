package org.specs2.specification.core

import org.specs2.concurrent.{ExecutorServices, ExecutionEnv}
import org.specs2.specification.AfterAll

import scala.concurrent.ExecutionContext

/**
 * This trait copies the inherited env: Env for a Specification and makes sure it is shutdown
 * when the specification finishes
 *
 * The specification using this trait must require an Env:
 *
 * class MySpec(env: Env) extends Specification with OwnEnv
 */
trait OwnEnv extends AfterAll:

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

  def afterAll(): Unit =
    ownEnv.shutdown()
