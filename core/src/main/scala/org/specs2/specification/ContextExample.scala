package org.specs2
package specification

import core._
import create.{ContextualFragmentFactory, FragmentsFactory}
import execute._

/**
 * For each created example use a given context
 */
trait ContextExample extends FragmentsFactory { outer =>
  protected def context: Context

  override protected def fragmentFactory = new ContextualFragmentFactory(super.fragmentFactory, context)
}

/**
 * For each created example use a given before action
 */
trait BeforeExample extends ContextExample { outer =>
  protected def before: Any
  protected def context = new Before { def before = outer.before }
}

/**
 * For each created example use a given after action
 */
trait AfterExample extends ContextExample { outer =>
  protected def after: Any
  protected def context = new After { def after = outer.after }
}

/**
 * For each created example use a given before action
 */
trait BeforeAfterExample extends ContextExample { outer =>
  protected def before: Any

  protected def after: Any

  protected def context = new BeforeAfter {
    def before = outer.before
    def after = outer.after
  }
}

/**
 * For each created example use a given around action
 */
trait AroundExample extends ContextExample { outer =>
  protected def around[R : AsResult](r: =>R): Result
  protected def context = new Around { def around[R : AsResult](r: =>R) = outer.around(r) }
}

/**
 * For each created example use a given fixture object
 */
trait FixtureExample[T] extends FragmentsFactory { outer =>
  protected def fixture[R : AsResult](f: T => R): Result

  implicit def fixtureFunctionToResult[R : AsResult]: AsResult[T => R] = new AsResult[T => R] {
    def asResult(f: =>(T => R)) = fixture(f)
  }
}

/**
 * Execute a step before all fragments
 */
trait BeforeAll extends SpecificationStructure with FragmentsFactory {
  def beforeAll: Unit
  override def map(fs: =>core.Fragments) = fs.prepend(fragmentFactory.Step(beforeAll))
}

/**
 * Execute a step after all fragments
 */
trait AfterAll extends SpecificationStructure with FragmentsFactory {
  def afterAll: Unit
  override def map(fs: =>core.Fragments) = fs.append(fragmentFactory.Step(afterAll))
}

/**
 * Execute a step before and after all fragments
 */
trait BeforeAfterAll extends SpecificationStructure with FragmentsFactory {
  def beforeAll: Unit
  def afterAll: Unit
  override def map(fs: =>core.Fragments) = fs.prepend(fragmentFactory.Step(beforeAll)).append(fragmentFactory.Step(afterAll))
}

/**
 * Execute some fragments before all others
 */
trait BeforeSpec extends SpecificationStructure {
  def beforeSpec: core.Fragments
  override def map(fs: =>core.Fragments) = fs.prepend(beforeSpec)
}

/**
 * Execute some fragments after all others
 */
trait AfterSpec extends SpecificationStructure {
  def afterSpec: core.Fragments
  override def map(fs: =>core.Fragments) = fs.append(afterSpec)
}

/**
 * Execute some fragments before and after all others
 */
trait BeforeAfterSpec extends SpecificationStructure {
  def beforeSpec: core.Fragments
  def afterSpec: core.Fragments
  override def map(fs: =>core.Fragments) = fs.prepend(beforeSpec).append(afterSpec)
}
