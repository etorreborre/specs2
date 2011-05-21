package org.specs2
package specification

import control.Exceptions._
import execute.FailureException._
import execute.SkipException._
import execute.Error._
import matcher.MatchResult
import execute.{FailureException, SkipException, Error, Result}

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
  implicit def doBefore[T <% Result](t: =>T) = new BeforeResult(t)
  class BeforeResult[T <% Result](t: =>T) {
    def before(action: => Unit) = new Before {
      def before = action
    }.apply(t)
  }

  /**
   * add an Around function to any kind of result
   */
  implicit def doAround[T <% Result](t: =>T) = new AroundResult(t)
  class AroundResult[T <% Result](t: =>T) {
    def around(f: Result => Result) = new Around {
      def around[R](r: =>R)(implicit conv: R => Result): Result = f(conv(r))
    }.apply(t)
  }

  /**
   * add an after action to any kind of result
   */
  implicit def doAfter[T <% Result](t: =>T) = new AfterResult(t)
  class AfterResult[T <% Result](t: =>T) {
    def after(action: => Unit) = new After {
      def after = action
    }.apply(t)
  }
}

object Contexts extends Contexts