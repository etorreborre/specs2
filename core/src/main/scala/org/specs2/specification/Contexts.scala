package org.specs2
package specification

import core.*
import execute.*
import specification.create.*
import data.AlwaysTag
import scala.concurrent.{Future, ExecutionContext}
import ResourceType.*
import StandardResults.*

/** Run a given fragment before each fragment
  */
trait BeforeEach extends SpecificationStructure:
  protected def before: Fragments

  override def flatMap(f: Fragment): Fragments =
    before.append(f)

/** Run a given fragment after each fragment
  */
trait AfterEach extends SpecificationStructure:
  protected def after: Fragments

  override def flatMap(f: Fragment): Fragments =
    after.prepend(f)

/** Run a given fragment before and after each fragment
  */
trait BeforeAfterEach extends SpecificationStructure:
  protected def before: Fragments
  protected def after: Fragments

  override def flatMap(f: Fragment): Fragments =
    before.append(f).append(after)

/** Run a function around each execution result
  */
trait AroundEach extends SpecificationStructure:
  protected def around[T: AsResult](t: =>T): Result

  override def flatMap(f: Fragment): Fragments =
    f.updateResult(around)

/** For each created example use a given fixture object
  */
trait ForEach[T]:

  protected def foreach[R: AsExecution](f: T => R): R

  given [R: AsExecution]: AsExecution[T => R] with
    def execute(f: => (T => R)): Execution =
      AsExecution[R].execute(foreach(f))

/** Acquire a resource for the whole spec and release it at the end
  */
trait Resource[T] extends BeforeAfterSpec with FragmentsFactory:

  protected def acquire: Future[T]

  protected def resourceKey: Option[String] =
    None

  private def getResourceKey: String =
    resourceKey.getOrElse(getClass.getName + "-" + hashCode.toString)

  protected def release(resource: T): Execution

  def beforeSpec =
    fragmentFactory.step(Execution.withEnvAsync { env =>
      implicit val ec = env.executionContext
      lazy val acquired: Future[T] = acquire.recoverWith { case e: Exception =>
        Future.failed[T](new Exception("resource unavailable", e))
      }

      resourceKey match
        // local resource
        case None =>
          acquired.map(r => { env.resources.addOne(getResourceKey -> ResourceExecution(Local, r, release(r))); () })
        // global resource, only acquire it if not acquired before
        case Some(key) =>
          env.resources.get(key) match
            case Some(r) =>
              Future.successful(())
            case None =>
              acquired.map(r => { env.resources.addOne(key -> ResourceExecution(Global, r, release(r))); () })
    }.setErrorAsFatal)

  def afterSpec =
    Fragments(
      fragmentFactory.break,
      fragmentFactory.step {
        Execution.withEnvFlatten { env =>
          implicit val ec = env.executionContext
          env.resources.get(getResourceKey) match
            case None =>
              // we can assume here that if no resource was available for the key, that's because
              // it could not be acquired in the first place
              success
            case Some(ResourceExecution(Local, _, finalization)) =>
              env.resources.remove(getResourceKey)
              finalization
            case Some(ResourceExecution(_, _, _)) =>
              success
        }
      }
    )

  given [R: AsExecution]: AsExecution[T => R] with
    def execute(f: => (T => R)): Execution =
      Execution.withEnvFlatten { env =>
        env.resources.get(getResourceKey) match
          case Some(r) =>
            AsExecution[R].execute(f(r.resource.asInstanceOf[T]))
          case _ =>
            Execution.result(StandardResults.skipped("resource unavailable"))
      }

/** Execute some fragments before all others
  */
trait BeforeSpec extends SpecificationStructure with FragmentsFactory:
  def beforeSpec: Fragments

  override def map(fs: =>Fragments): Fragments =
    super.map(fs).prepend(beforeSpec.append(fragmentFactory.markAs(AlwaysTag)))

/** Execute some fragments after all others
  */
trait AfterSpec extends SpecificationStructure with FragmentsFactory:
  def afterSpec: Fragments

  override def map(fs: =>Fragments): Fragments =
    super.map(fs).append(afterSpec.append(fragmentFactory.markAs(AlwaysTag)))

/** Execute some fragments before and after all others
  */
trait BeforeAfterSpec extends SpecificationStructure with FragmentsFactory:
  def beforeSpec: Fragments
  def afterSpec: Fragments

  override def map(fs: =>Fragments): Fragments =
    super
      .map(fs)
      .prepend(beforeSpec.append(fragmentFactory.markAs(AlwaysTag)))
      .append(afterSpec.append(fragmentFactory.markAs(AlwaysTag)))

/** Execute a step before all other fragments
  */
@deprecated(since="5.0.0")
trait BeforeAll extends SpecificationStructure with FragmentsFactory:
  def beforeAll(): Unit

  override def map(fs: =>Fragments): Fragments =
    super.map(fs).prepend(fragmentFactory.step(beforeAll())).append(fragmentFactory.markAs(AlwaysTag))

/** Execute a step after all other fragments
  */
@deprecated(since="5.0.0")
trait AfterAll extends SpecificationStructure with FragmentsFactory:
  def afterAll(): Unit

  override def map(fs: =>Fragments): Fragments =
    super.map(fs).append(fragmentFactory.step(afterAll())).append(fragmentFactory.markAs(AlwaysTag))

/** Execute a step before and after all other fragments
  */
@deprecated(since="5.0.0")
trait BeforeAfterAll extends SpecificationStructure with FragmentsFactory:
  def beforeAll(): Unit
  def afterAll(): Unit

  override def map(fs: =>Fragments): Fragments =
    super
      .map(fs)
      .prepend(fragmentFactory.step(beforeAll())).append(fragmentFactory.markAs(AlwaysTag))
      .append(fragmentFactory.step(afterAll())).append(fragmentFactory.markAs(AlwaysTag))
