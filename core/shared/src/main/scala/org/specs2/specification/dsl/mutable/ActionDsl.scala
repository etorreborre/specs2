package org.specs2
package specification
package dsl
package mutable

import execute.{Error, Result}
import core._

/**
 * Dsl to create actions in a mutable spec
 */
trait ActionDsl extends FragmentBuilder with org.specs2.specification.dsl.ActionDsl:

  override def action[T : AsExecution](a: =>T): Fragment =
    addFragment(super.action(a))

  override def step[T : AsExecution](a: =>T): Fragment =
    step(a, global = true)

  override def step[T : AsExecution](a: =>T, global: Boolean): Fragment =
    addFragment(super.step(a).makeGlobal(global))

  def stopWhen(r: Result): Fragment = addFragment(fragmentFactory.step(()).stopOn(r))
  def stopWhen(f: Result => Boolean): Fragment = addFragment(fragmentFactory.step(()).stopWhen(f))

  def stopWhenSkipped: Fragment = stopWhenSkipped(true)
  def stopWhenSkipped(when: =>Boolean): Fragment =
    addFragment(fragmentFactory.step(()).stopWhen((r: Result) => mustStop(when).fold(_ => true, b => b && r.isSkipped)))

  def stopWhenFail: Fragment = stopWhenFail(true)
  def stopWhenFail(when: =>Boolean): Fragment =
    addFragment(fragmentFactory.step(()).stopWhen((r: Result) => mustStop(when).fold(_ => true, b => b && r.isFailure)))

  private def mustStop(when: =>Boolean) =
    try Right(when) catch { case e: Throwable => Left(Error(e)) }
