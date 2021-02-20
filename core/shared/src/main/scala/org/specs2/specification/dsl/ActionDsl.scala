package org.specs2
package specification
package dsl

import core.*
import create.FragmentsFactory

/**
 * Action Dsl for mutable specifications
 */
trait ActionDsl extends FragmentsFactory:

  def step[T : AsExecution](a: =>T): Fragment =
    fragmentFactory.step(a)

  def step[T : AsExecution](a: =>T, global: Boolean): Fragment =
    fragmentFactory.step(a)

  def action[T : AsExecution](a: =>T): Fragment =
    fragmentFactory.action(a)
