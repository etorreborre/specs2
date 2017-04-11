package org.specs2
package data

import matcher._
import org.specs2.fp._

class MonoidxSpec extends mutable.Spec with FpMatchers with AnyMatchers {
  "There is a Monoid for a Map if there is a Monoid for the values" >>
    Monoid.mapMonoid[String, Int].isMonoid
}
