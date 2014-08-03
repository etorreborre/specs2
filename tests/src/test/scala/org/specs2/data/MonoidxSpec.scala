package org.specs2
package data

import matcher._
import scalaz._
import Scalaz._

class MonoidxSpec extends mutable.Specification with ScalazMatchers {
  "There is a Monoid for a Map if there is a Monoid for the values" >>
    implicitly[Monoid[Map[String, Int]]].isMonoid
}
