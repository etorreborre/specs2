package org.specs2
package specification
package dsl
package mutable

import execute.{Error, Result}
import specification.core.Fragment
import specification.create.FragmentsFactory

trait ActionDsl extends FragmentBuilder with FragmentsFactory {
  def step(a: =>Any, global: Boolean = false) = addFragment(fragmentFactory.step(a).makeGlobal(global))

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

}

