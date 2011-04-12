package org.specs2
package specification

import execute.Result

/**
 * For each created example use a given Before context
 */
trait BeforeContextExample extends FragmentsBuilder {
  protected def beforeContext: Before

  private def parentExampleFactory = super.exampleFactory
  private[specs2] override def exampleFactory: ExampleFactory = new DecoratedExampleFactory(parentExampleFactory, beforeContext)
}
/**
 * For each created example use a given after method
 */
trait BeforeExample extends BeforeContextExample { outer =>
  protected def before: Any
  def beforeContext: Before = new Before { def before = outer.before }
}
/**
 * For each created example use a given After context
 */
trait AfterContextExample extends FragmentsBuilder {
  protected def afterContext: After

  private def parentExampleFactory = super.exampleFactory
  private[specs2] override def exampleFactory: ExampleFactory = new DecoratedExampleFactory(parentExampleFactory, afterContext)
}
/**
 * For each created example use a given after method
 */
trait AfterExample extends AfterContextExample { outer =>
  protected def after: Any
  def afterContext: After = new After { def after = outer.after }
}
/**
 * For each created example use a given Around context
 */
trait AroundContextExample extends FragmentsBuilder {
  protected def aroundContext: Around

  private def parentExampleFactory = super.exampleFactory
  private[specs2] override def exampleFactory: ExampleFactory = new DecoratedExampleFactory(parentExampleFactory, aroundContext)
}
/**
 * For each created example use a given around method
 */
trait AroundExample extends AroundContextExample { outer =>
  protected def around[T <% Result](t: =>T): Result
  def aroundContext: Around = new Around {
    def around[T <% Result](t: =>T) = outer.around(t)
  }
}
/**
 * For each created example use a given BeforeAfter context
 */
trait BeforeAfterContextExample extends FragmentsBuilder {
  protected def beforeAfterContext: BeforeAfter

  private def parentExampleFactory = super.exampleFactory
  private[specs2] override def exampleFactory: ExampleFactory = new DecoratedExampleFactory(parentExampleFactory, beforeAfterContext)
}
/**
 * For each created example use a given before/after method
 */
trait BeforeAfterExample extends BeforeAfterContextExample { outer =>
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
trait BeforeAfterAroundContextExample extends FragmentsBuilder {
  protected def beforeAfterAroundContext: BeforeAfterAround

  private def parentExampleFactory = super.exampleFactory
  private[specs2] override def exampleFactory: ExampleFactory = new DecoratedExampleFactory(parentExampleFactory, beforeAfterAroundContext)
}
/**
 * For each created example use a given before/after method
 */
trait BeforeAfterAroundExample extends BeforeAfterAroundContextExample { outer =>
  protected def before: Any
  protected def after: Any
  protected def around[T <% Result](t: =>T): Result
  def beforeAfterAroundContext: BeforeAfterAround = new BeforeAfterAround {
    def before = outer.before
    def after = outer.after
    def around[T <% Result](t: =>T): Result = outer.around(t)
  }
}
