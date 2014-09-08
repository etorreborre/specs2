package org.specs2
package specification
package dsl

import create.FragmentsFactory

/**
 * Action Dsl for mutable specifications
 */
trait ActionDsl extends FragmentsFactory {
  def step(a: =>Any) = fragmentFactory.step(a)
  def step(a: =>Any, global: Boolean = false) = fragmentFactory.step(a).makeGlobal(global)
  def action(a: =>Any) = fragmentFactory.action(a)

  @deprecated(message = "use step instead", since = "3.0")
  def Step(a: =>Any) = fragmentFactory.step(a)

  @deprecated(message = "use step instead", since = "3.0")
  def Step(a: =>Any, global: Boolean = false) = fragmentFactory.step(a).makeGlobal(global)

  @deprecated(message = "use action instead", since = "3.0")
  def Action(a: =>Any) = fragmentFactory.action(a)

}
