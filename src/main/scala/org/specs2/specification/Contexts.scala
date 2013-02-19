package org.specs2
package specification

import execute.{AsResult, Result}

/**
 * This trait is only used when we wish to write after actions in unit specifications like this
 *
 * "an example" ! {
 *   1 must_== 1
 * }.after(clean)
 */
trait Contexts {
  /**
   * add a before action to any kind of result
   */
  implicit def doBefore[T : AsResult](t: =>T) = new BeforeResult(t)
  class BeforeResult[T : AsResult](t: =>T) {
    def before(action: => Unit) = new Before {
      def before = action
    }.apply(t)
  }

  /**
   * add an Around function to any kind of result
   */
  implicit def doAround[T : AsResult](t: =>T) = new AroundResult(t)
  class AroundResult[T : AsResult](t: =>T) {
    def around(f: Result => Result) = new Around {
      def around[R : AsResult](r: =>R): Result = f(AsResult(r))
    }.apply(t)
  }

  /**
   * add an after action to any kind of result
   */
  implicit def doAfter[T : AsResult](t: =>T) = new AfterResult(t)
  class AfterResult[T : AsResult](t: =>T) {
    def after(action: => Unit) = new After {
      def after = action
    }.apply(t)
  }

  protected[specs2] val defaultContext = new Context { def apply[T : AsResult](a: =>T): Result = AsResult(a) }

}

/**
 * Use this trait to deactivate the Contexts implicits
 */
trait NoContexts extends Contexts {
  override def doBefore[T : AsResult](t: =>T) = super.doBefore(t)
  override def doAround[T : AsResult](t: =>T) = super.doAround(t)
  override def doAfter[T : AsResult](t: =>T) = super.doAfter(t)
}

object Contexts extends Contexts