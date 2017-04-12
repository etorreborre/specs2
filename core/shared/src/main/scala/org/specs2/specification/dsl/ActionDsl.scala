package org.specs2
package specification
package dsl

import create.FragmentsFactory

/**
 * Action Dsl for mutable specifications
 */
trait ActionDsl extends FragmentsFactory {
  def step(a: =>Any) = fragmentFactory.step(a)
  def step(a: =>Any, global: Boolean) = fragmentFactory.step(a).makeGlobal(global)
  def action(a: =>Any) = fragmentFactory.action(a)
}
