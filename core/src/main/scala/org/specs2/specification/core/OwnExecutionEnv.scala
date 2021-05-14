package org.specs2
package specification
package core

import concurrent.ExecutionEnv
import dsl.*

import scala.concurrent.ExecutionContext

/**
 * This trait provides an isolated Execution Env for a Specification and makes sure it is shutdown
 * when the specification finishes.
 *
 * The specification using this trait must require an Env:
 *
 * class MySpec(env: Env) extends Specification with OwnExecutionEnv
 */
trait OwnExecutionEnv extends AfterSpec:
  self: FragmentsDsl =>

  def env: Env

  private lazy val ownEnv: Env =
    env.copy(
      executionEnv =       ExecutionEnv.create(env.arguments, env.systemLogger, tag = Some(getClass.getName)),
      specs2ExecutionEnv = ExecutionEnv.createSpecs2(env.arguments, env.systemLogger, tag = Some(getClass.getName)))

  given ee: ExecutionEnv =
    ownEnv.executionEnv

  lazy val ec: ExecutionContext =
    ownEnv.executionContext

  def afterSpec: Fragments =
    step(ownEnv.shutdown())
