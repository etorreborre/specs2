package org.specs2
package specification

import core._
import org.specs2.specification.create.{InterpolatedFragment, S2StringContext, ContextualFragmentFactory, FragmentsFactory}
import execute._
import org.specs2.main.CommandLine

/**
 * For each created example use a given context
 */
trait EachContext extends FragmentsFactory { outer =>
  protected def context: Env => Context

  override protected def fragmentFactory = new ContextualFragmentFactory(super.fragmentFactory, context)
}

/**
 * For each created example use a context using the command line arguments
 */
trait CommandLineArgumentsInContext extends FragmentsFactory { outer =>
  protected def context: CommandLine => Context
  override protected def fragmentFactory = new ContextualFragmentFactory(super.fragmentFactory, (env: Env) => context(env.arguments.commandLine))
}

/**
 * For each created example use a given before action
 */
trait BeforeEach extends EachContext { outer =>
  protected def before: Any
  protected def context = (env: Env) => new Before { def before = outer.before }
}

/**
 * For each created example use a given after action
 */
trait AfterEach extends EachContext { outer =>
  protected def after: Any
  protected def context = (env: Env) => new After { def after = outer.after }
}

/**
 * For each created example use a given before action
 */
trait BeforeAfterEach extends EachContext { outer =>
  protected def before: Any
  protected def after: Any

  protected def context = (env: Env) => new BeforeAfter {
    def before = outer.before
    def after = outer.after
  }
}

/**
 * For each created example use a given around action
 */
trait AroundEach extends EachContext { outer =>
  protected def around[R : AsResult](r: =>R): Result
  protected def context = (env: Env) => new Around { def around[R : AsResult](r: =>R) = outer.around(r) }
}

/**
 * For each created example use a given fixture object
 */
trait ForEach[T] extends EachContext { outer =>
  protected def foreach[R : AsResult](f: T => R): Result

  protected def context: Env => Context = (env: Env) => new Around {
    def around[R : AsResult](r: =>R) = AsResult(r)
  }

  implicit def foreachFunctionToResult[R : AsResult]: AsResult[T => R] = new AsResult[T => R] {
    def asResult(f: =>(T => R)) = foreach(f)
  }
}

/**
 * For each example but inject data depending on command line arguments
 */
trait ForEachWithCommandLineArguments[T] extends EachContext { outer: S2StringContext =>
  protected def foreach[R : AsResult](commandLine: CommandLine)(f: T => R): Result

  protected def context: Env => Context = (env: Env) => new Around {
    def around[R : AsResult](r: =>R) = AsResult(r)
  }

  implicit def foreachFunctionToExecution[R : AsResult](f: T => R): Execution =
    Execution.withEnv((env: Env) => foreach(env.arguments.commandLine)(f))

  implicit def foreachFunctionIsInterpolatedFragment[R : AsResult](f: =>(T => R)): InterpolatedFragment = new InterpolatedFragment {
    def append(fs: Fragments, text: String, start: Location, end: Location, expression: String) = {
      val (description, before) = descriptionAndBefore(text, start, end, expression)
      fs append before append ff.example(description, foreachFunctionToExecution(f)).setLocation(end)
    }
  }
}

/**
 * Execute a step before all fragments
 */
trait BeforeAll extends SpecificationStructure with FragmentsFactory {
  def beforeAll(): Unit
  override def map(fs: =>core.Fragments) = super.map(fs).prepend(fragmentFactory.step(beforeAll))
}

/**
 * Execute a step after all fragments
 */
trait AfterAll extends SpecificationStructure with FragmentsFactory {
  def afterAll(): Unit
  override def map(fs: =>core.Fragments) = super.map(fs).append(fragmentFactory.step(afterAll))
}

/**
 * Execute a step before and after all fragments
 */
trait BeforeAfterAll extends SpecificationStructure with FragmentsFactory {
  def beforeAll(): Unit
  def afterAll(): Unit
  override def map(fs: =>core.Fragments) = super.map(fs).prepend(fragmentFactory.step(beforeAll)).append(fragmentFactory.step(afterAll))
}

/**
 * Execute some fragments before all others
 */
trait BeforeSpec extends SpecificationStructure {
  def beforeSpec: core.Fragments
  override def map(fs: =>core.Fragments) = super.map(fs).prepend(beforeSpec)
}

/**
 * Execute some fragments after all others
 */
trait AfterSpec extends SpecificationStructure {
  def afterSpec: core.Fragments
  override def map(fs: =>core.Fragments) = super.map(fs).append(afterSpec)
}

/**
 * Execute some fragments before and after all others
 */
trait BeforeAfterSpec extends SpecificationStructure {
  def beforeSpec: core.Fragments
  def afterSpec: core.Fragments
  override def map(fs: =>core.Fragments) = super.map(fs).prepend(beforeSpec).append(afterSpec)
}

/**
 * DEPRECATED TRAITS
 */
@deprecated("use the BeforeEach trait instead", "3.0")
trait BeforeExample extends BeforeEach

@deprecated("use the AfterEach trait instead", "3.0")
trait AfterExample extends AfterEach

@deprecated("use the AroundEach trait instead", "3.0")
trait AroundExample extends AroundEach

@deprecated("use the BeforeAfterEach trait instead", "3.0")
trait BeforeAfterExample extends BeforeAfterEach

@deprecated("use the ForEach trait instead", "3.0")
trait FixtureExample[T] extends ForEach[T]
