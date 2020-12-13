package org.specs2
package specification

import core._
import execute._
import specification.create._
import data.AlwaysTag
import scala.concurrent.{Future, ExecutionContext}

/**
 * Run a given fragment before each fragment
 */
trait BeforeEach extends SpecificationStructure:
  protected def before: Fragments

  override def flatMap(f: Fragment): Fragments =
    before.append(f)

/**
 * Run a given fragment after each fragment
 */
trait AfterEach extends SpecificationStructure:
  protected def after: Fragments

  override def flatMap(f: Fragment): Fragments =
    after.prepend(f)

/**
 * Run a given fragment before and after each fragment
 */
trait BeforeAfterEach extends SpecificationStructure:
  protected def before: Fragments
  protected def after: Fragments

  override def flatMap(f: Fragment): Fragments =
    before.append(f).append(after)

/**
 * Run a function around each execution result
 */
trait AroundEach extends SpecificationStructure:
  protected def around[T : AsResult](t: =>T): Result

  override def flatMap(f: Fragment): Fragments =
    f.updateResult(around)

/**
 * For each created example use a given fixture object
 */
trait ForEach[T]:

  protected def foreach[R : AsExecution](f: T => R): R

  given [R : AsExecution] as AsExecution[T => R]:
    def execute(f: =>(T => R)): Execution =
      AsExecution[R].execute(foreach(f))

/**
 * Acquire a resource for the whole spec and release it at the end
 */
trait Resource[T] extends BeforeAfterSpec with FragmentsFactory with StandardResults:

  protected def acquire: Future[T]

  protected def release(resource: T): Execution

  def beforeSpec =
    fragmentFactory.step(Execution.withEnvAsync { env =>
      implicit val ec = env.executionContext
      acquire.map(r => env.resource.set(r))
    }.setErrorAsFatal)

  def afterSpec =
    Fragments(fragmentFactory.break, fragmentFactory.step(release))

  given [R : AsExecution] as AsExecution[T => R]:
    def execute(f: =>(T => R)): Execution =
      Execution.withEnvFlatten { env =>
        env.resource.toOption match
          case Some(t) =>
            AsExecution[R].execute(f(t.asInstanceOf[T]))
          case None =>
            Execution.result(skipped("resource unavailable"))
      }

/**
 * Execute some fragments before all others
 */
trait BeforeSpec extends SpecificationStructure:
  def beforeSpec: Fragments

  override def map(fs: =>Fragments): Fragments =
    super.map(fs).prepend(beforeSpec)

/**
 * Execute some fragments after all others
 */
trait AfterSpec extends SpecificationStructure:
  def afterSpec: Fragments

  override def map(fs: =>Fragments): Fragments =
    super.map(fs).append(afterSpec)

/**
 * Execute some fragments before and after all others
 */
trait BeforeAfterSpec extends SpecificationStructure:
  def beforeSpec: Fragments
  def afterSpec: Fragments

  override def map(fs: =>Fragments): Fragments =
    super.map(fs).prepend(beforeSpec).append(afterSpec)
