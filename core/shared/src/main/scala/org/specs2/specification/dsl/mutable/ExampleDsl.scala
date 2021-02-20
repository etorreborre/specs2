package org.specs2
package specification
package dsl
package mutable

import org.specs2.execute.AsResult
import control.{ImplicitParameters, Use}
import ImplicitParameters.*
import org.specs2.specification.core.*
import org.specs2.specification.script.StepParser
import scala.util.NotGiven

/**
 * Dsl for creating examples in a mutable specification
 */
trait ExampleDsl extends dsl.ExampleDsl with BlockCreation:

  override def addExample(d: String, execution: =>Execution): Fragment =
    addFragment(super.addExample(d, execution))
    addFragment(fragmentFactory.break)
