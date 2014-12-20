package org.specs2
package matcher

import org.scalacheck.Prop
import execute.AsResult
import org.specs2.scalacheck.Parameters


/**
 * This trait can be mixed in a Specification to avoid counting the number of times that a property was executed as the
 * number of expectations. With this trait we just count 1 for each result
 */
trait OneExpectationPerProp extends ScalaCheckMatchers {
  private def superPropAsResult[P <: Prop] = super.propAsResult[P]

  override implicit def propAsResult[P <: Prop](implicit p: Parameters): AsResult[P] = new AsResult[P] {
    def asResult(prop: =>P) = superPropAsResult.asResult(prop).setExpectationsNb(1)
  }
}

