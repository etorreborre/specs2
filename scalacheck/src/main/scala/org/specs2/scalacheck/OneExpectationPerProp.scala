package org.specs2
package scalacheck

import org.scalacheck.{Properties, Prop}
import org.scalacheck.util.{Pretty, FreqMap}
import org.specs2.execute.AsResult

/**
 * This trait can be mixed in a Specification to avoid counting the number of times that a property was executed as the
 * number of expectations. With this trait we just count 1 for each result
 */
trait OneExpectationPerProp extends AsResultProp {
  private def superPropAsResult = super.propAsResult
  private def superPropertiesAsResult = super.propertiesAsResult

  implicit override def propAsResult(implicit p: Parameters, pfq: FreqMap[Set[Any]] => Pretty): AsResult[Prop] = new AsResult[Prop] {
    def asResult(prop: =>Prop) = superPropAsResult.asResult(prop).setExpectationsNb(1)
  }

  implicit override def propertiesAsResult(implicit p: Parameters, pfq: FreqMap[Set[Any]] => Pretty): AsResult[Properties] = new AsResult[Properties] {
    def asResult(properties: =>Properties) = superPropertiesAsResult.asResult(properties).setExpectationsNb(1)
  }
}
