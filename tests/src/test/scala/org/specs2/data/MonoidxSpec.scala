package org.specs2
package data

import matcher.*
import org.specs2.fp.*

class MonoidxSpec extends mutable.Spec with FpMatchers with AnyMatchers:
  "There is a Monoid for a Map if there is a Monoid for the values" >>
    Monoid.mapMonoid[String, Int].isMonoid.set(minTestsOk = 5)
