package org.specs2
package specification
package dsl

import create.FragmentsFactory

trait ActionDsl extends FragmentsFactory {
  def step(a: =>Any) = fragmentFactory.step(a)
  def step(a: =>Any, global: Boolean = false) = fragmentFactory.step(a).makeGlobal(global)
  def action(a: =>Any) = fragmentFactory.action(a)
}
