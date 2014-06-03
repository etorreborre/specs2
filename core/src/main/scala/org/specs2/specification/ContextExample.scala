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
 * Execute some fragments before all others
 */
trait BeforeAll extends SpecificationStructure {
  def beforeAll: core.Fragments
  override def map(fs: =>core.Fragments) = fs.prepend(beforeAll)
}

/**
 * Execute some fragments after all others
 */
trait AfterAll extends SpecificationStructure {
  def afterAll: core.Fragments
  override def map(fs: =>core.Fragments) = fs.append(afterAll)
}

/**
 * Execute some fragments before and after all others
 */
trait BeforeAfterAll extends SpecificationStructure {
  def beforeAll: core.Fragments
  def afterAll: core.Fragments
  override def map(fs: =>core.Fragments) = fs.prepend(beforeAll).append(afterAll)
}
