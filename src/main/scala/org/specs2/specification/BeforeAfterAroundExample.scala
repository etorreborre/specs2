package org.specs2
package specification

import execute.{AsResult, Result}

/**
 * For each created example use a given Before context
 */
trait BeforeContextExample[C <: Before] extends ExamplesFactory {
  protected def beforeContext: C
  override def exampleFactory: ExampleFactory = new DecoratedExampleFactory(super.exampleFactory, beforeContext)
}
/**
 * For each created example use a given after method
 */
trait BeforeExample extends BeforeContextExample[Before] { outer =>
  protected def before: Any
  def beforeContext: Before = new Before { def before = outer.before }
}
/**
 * For each created example use a given After context
 */
trait AfterContextExample[C <: After] extends ExamplesFactory {
  protected def afterContext: C
  override def exampleFactory: ExampleFactory = new DecoratedExampleFactory(super.exampleFactory, afterContext)
}
/**
 * For each created example use a given after method
 */
trait AfterExample extends AfterContextExample[After] { outer =>
  protected def after: Any
  def afterContext: After = new After { def after = outer.after }
}
/**
 * For each created example use a given Around context
 */
trait AroundContextExample[C <: Around] extends ExamplesFactory {
  protected def aroundContext: C
  override def exampleFactory: ExampleFactory = new DecoratedExampleFactory(super.exampleFactory, aroundContext)
}
/**
 * For each created example use a given around method
 */
trait AroundExample extends AroundContextExample[Around] { outer =>
  protected def around[T : AsResult](t: =>T): Result
  def aroundContext: Around = new Around {
    def around[T : AsResult](t: =>T) = outer.around(t)
  }
}
/**
 * For each created example use a given BeforeAfter context
 */
trait BeforeAfterContextExample[C <: BeforeAfter] extends ExamplesFactory {
  protected def beforeAfterContext: C
  override def exampleFactory: ExampleFactory = new DecoratedExampleFactory(super.exampleFactory, beforeAfterContext)
}
/**
 * For each created example use a given before/after method
 */
trait BeforeAfterExample extends BeforeAfterContextExample[BeforeAfter] { outer =>
  protected def before: Any
  protected def after: Any
  def beforeAfterContext: BeforeAfter = new BeforeAfter {
    def before = outer.before
    def after = outer.after
  }
}
/**
 * For each created example use a given BeforeAfterAround context
 */
trait BeforeAfterAroundContextExample[C <: BeforeAfterAround] extends ExamplesFactory {
  protected def beforeAfterAroundContext: C
  override def exampleFactory: ExampleFactory = new DecoratedExampleFactory(super.exampleFactory, beforeAfterAroundContext)
}
/**
 * For each created example use a given before/after method
 */
trait BeforeAfterAroundExample extends BeforeAfterAroundContextExample[BeforeAfterAround] { outer =>
  protected def before: Any
  protected def after: Any
  protected def around[T : AsResult](t: =>T): Result
  def beforeAfterAroundContext: BeforeAfterAround = new BeforeAfterAround {
    def before = outer.before
    def after = outer.after
    def around[T : AsResult](t: =>T): Result = outer.around(t)
  }
}

/**
 * For each created example use a given after method
 */
trait FixtureExample[T] {
  protected def fixture[R : AsResult](f: T => R): Result
  implicit def fixtureContext = new Fixture[T] { def apply[R : AsResult](f: T => R) = fixture(f) }
}
