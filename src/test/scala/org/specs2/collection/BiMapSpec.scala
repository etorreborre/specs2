package org.specs2
package collection

import specification.Grouped
import matcher._
import BiMap._

class BiMapSpec extends Specification with BiMapExamples { def is =

 "a BiMap can be defined with a sequence of keys and values"    ! g1.e1^
 "it is possible to get the value corresponding to a given key" ! g1.e2^
 "it is possible to get the key corresponding to a given value" ! g1.e3^
 "it is possible to know if a value exists"                     ! g1.e4^
 "it is possible to know if a key exists"                       ! g1.e5^
 end

}

trait BiMapExamples extends Grouped with MustMatchers {
  val bimap = Seq("one" <-> 1, "two" <-> 2, "three" <-> 3)

  "BiMap api" - new g1 {
    e1 = bimap.values === Seq(1, 2, 3)
    e2 = bimap.fromKey("one") === Some(1)
    e3 = bimap.fromValue(2) === Some("two")
    e4 = bimap.containsValue(3)
    e5 = bimap.containsKey("three")
  }
}
