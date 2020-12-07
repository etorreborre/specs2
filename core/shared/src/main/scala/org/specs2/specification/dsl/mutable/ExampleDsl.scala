package org.specs2
package specification
package dsl
package mutable

import org.specs2.execute.AsResult
import control.{ImplicitParameters, Use}
import ImplicitParameters._
import org.specs2.specification.core._
import org.specs2.specification.script.StepParser
import scala.util.Not

/**
 * Dsl for creating examples in a mutable specification
 */
trait ExampleDsl extends dsl.ExampleDsl with BlockCreation:

  extension [S : AsExecution, R] (d: String)(using not: Not[NoBangExamples]):
    def !(s: S)(using t: ToBlock[S, R]): R =
     summon[ToBlock[S, R]].toBlock(d, s)
