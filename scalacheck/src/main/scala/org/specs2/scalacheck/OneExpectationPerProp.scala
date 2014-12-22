package org.specs2
package scalacheck

import org.scalacheck.Prop
import org.scalacheck.util.{Pretty, FreqMap}
import org.specs2.execute.AsResult

/**
 * This trait can be mixed in a Specification to avoid counting the number of times that a property was executed as the
 * number of expectations. With this trait we just count 1 for each result
 */
trait OneExpectationPerProp extends AsResultProp {
  private def superPropAsResult[P <: Prop] = super.propAsResult[P]

  implicit override def propAsResult[P <: Prop](implicit p: Parameters, pfq: FreqMap[Set[Any]] => Pretty): AsResult[P] = new AsResult[P] {
    def asResult(prop: =>P) = superPropAsResult.asResult(prop).setExpectationsNb(1)
  }
}
