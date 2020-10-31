package org.specs2
package scalacheck

import org.scalacheck.{Properties, Prop}
import org.scalacheck.util.{Pretty, FreqMap}
import org.specs2.execute.{AsResult, Result}

/**
 * This trait can be mixed in a Specification to avoid counting the number of times that a property was executed as the
 * number of expectations. With this trait we just count 1 for each result
 */
trait OneExpectationPerProp extends AsResultProp:
  private def superPropAsResult = super.propAsResult
  private def superPropertiesAsResult = super.propertiesAsResult

  given propAsResult1(using p: Parameters, pfq: FreqMap[Set[Any]] => Pretty) as AsResult[Prop]:
    def asResult(prop: =>Prop): Result = superPropAsResult.asResult(prop).setExpectationsNb(1)

  given propertiesAsResult1(using p: Parameters, pfq: FreqMap[Set[Any]] => Pretty) as AsResult[Properties]:
    def asResult(properties: =>Properties): Result =
      superPropertiesAsResult.asResult(properties).setExpectationsNb(1)
