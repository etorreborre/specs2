package org.specs2
package specification
package dsl
package mutable

import execute.{Error, Result}
import specification.core.Fragment
import specification.create.FragmentsFactory

trait ActionDsl extends FragmentBuilder with FragmentsFactory {
  def action(a: =>Any) = addFragment(fragmentFactory.Action(a))
  def step(a: =>Any, global: Boolean = false) = addFragment(fragmentFactory.Step(a).makeGlobal(global))

  def stopWhen(r: Result): Fragment = addFragment(fragmentFactory.Step(()).stopOn(r))
  def stopWhen(f: Result => Boolean): Fragment = addFragment(fragmentFactory.Step(()).stopWhen(f))

  def stopWhenSkipped: Fragment = stopWhenSkipped(true)
  def stopWhenSkipped(when: =>Boolean): Fragment =
    addFragment(fragmentFactory.Step(()).stopWhen((r: Result) => mustStop(when).fold(_ => true, b => b && r.isSkipped)))

  def stopWhenFail: Fragment = stopWhenFail(true)
  def stopWhenFail(when: =>Boolean): Fragment =
    addFragment(fragmentFactory.Step(()).stopWhen((r: Result) => mustStop(when).fold(_ => true, b => b && r.isFailure)))

  private def mustStop(when: =>Boolean) =
    try Right(when) catch { case e: Throwable => Left(Error(e)) }

}

