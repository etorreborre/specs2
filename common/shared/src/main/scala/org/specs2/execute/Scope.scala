package org.specs2.execute

/** This trait can be used in mutable specifications to provide setup values. For example:
  * ```scala
  *    class MySpec extends mutable.Specification:
  *       "e1" in new MyScope:
  *         someValue === 1
  *
  *     trait MyScope extends Scope: val someValue: Int = 1
  * ```
  */
trait Scope

object Scope:
  /** This Given transforms a Scope to a Result */
  given scopeAsResult[S <: Scope]: AsResult[S] = new AsResult[S]:
    def asResult(t: =>S): Result = AsResult.safely { Result.resultOrSuccess(t) }
