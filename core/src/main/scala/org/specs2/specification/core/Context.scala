package org.specs2
package specification

import execute.Result.*
import execute.{ResultExecution, Result, AsResult}
import fp.*

/** generic trait for Before, After, Around
  */
trait Context:
  def apply[T: AsResult](a: =>T): Result

object Context:
  def compose(c1: Context, c2: Context): Context =
    new Context:
      def apply[T: AsResult](a: =>T): Result = c1(c2(a))

/** The Before trait can be inherited by classes representing a context where an action must be executing before the
  * main executable action
  *
  * @see
  *   Example to understand why the type T must : AsResult
  */
trait Before extends Context:
  outer =>

  /** override this method to provide the before behavior */
  def before: Any

  /** execute an action returning a Result and finally the before action.
    *
    * The action will be aborted if the before block fails:
    *
    *   - with an exception
    *   - with a non-Success result
    *   - with a non-Success match result
    */
  override def apply[T: AsResult](a: =>T): Result =
    ResultExecution.execute(before)((any: Any) => AsResult(a))

  /** compose the actions of 2 Before traits */
  def compose(b: Before): Before =
    new Before:
      def before = { b.before; outer.before }

  /** sequence the actions of 2 Before traits */
  def andThen(b: Before): Before =
    new Before:
      def before = { outer.before; b.before }

object Before:
  def create(action: =>Any): Before =
    new Before:
      def before: Any = action

/** The After trait can be inherited by classes representing a context where an action must be executing after the main
  * executable action
  *
  * @see
  *   Example to understand why the type T must : AsResult
  */
trait After extends Context:
  outer =>

  /** override this method to provide the after behavior */
  def after: Any

  /** execute an action returning a Result and finally the after action
    */
  def apply[T: AsResult](a: =>T): Result =
    try { AsResult(a) }
    finally { after; () }

  /** compose the actions of 2 After traits */
  def compose(a: After): After =
    new After:
      def after = { a.after; outer.after }

  /** sequence the actions of 2 After traits */
  def andThen(b: After): After =
    new After:
      def after = { outer.after; b.after }

object After:
  def create(action: =>Any) =
    new After:
      def after: Any = action

trait BeforeAfter extends Before with After:
  outer =>

  override def apply[T: AsResult](a: =>T): Result =
    lazy val result = super[Before].apply(a)
    super[After].apply(result)

  /** compose the actions of 2 BeforeAfter traits */
  def compose(b: BeforeAfter): BeforeAfter =
    new BeforeAfter:
      def before = { b.before; outer.before }
      def after = { b.after; outer.after }

  /** sequence the actions of 2 BeforeAfter traits */
  def andThen(b: BeforeAfter): BeforeAfter =
    new BeforeAfter:
      def before = { outer.before; b.before }
      def after = { outer.after; b.after }

object BeforeAfter:
  def create(beforeAction: =>Any, afterAction: =>Any) =
    new BeforeAfter:
      def before: Any = beforeAction
      def after: Any = afterAction

/** The Around trait can be inherited by classes which will execute some code inside the around method provided by the
  * context.
  *
  * This can be used for example to execute some code inside a webapp session
  *
  * @see
  *   Example to understand why the type T must : AsResult
  */
trait Around extends Context:
  outer =>

  def around[T: AsResult](t: =>T): Result
  def apply[T: AsResult](a: =>T) = around(a)

  /** compose the actions of 2 Around traits */
  def compose(a: Around): Around =
    new Around:
      def around[T: AsResult](t: =>T): Result =
        a.around(outer.around(t))

  /** sequence the actions of 2 Around traits */
  def andThen(a: Around): Around =
    new Around:
      def around[T: AsResult](t: =>T): Result =
        outer.around(a.around(t))

object Around:
  def create(aroundAction: Result => Result) =
    new Around:
      def around[T: AsResult](t: =>T): Result =
        aroundAction(AsResult(t))
