package org.specs2
package collection

import specification._
import matcher._
import BiMap._

class BiMapSpec extends script.Specification with BiMapExamples { def is = s2"""

 Bimaps define bijection relationships between values

 + a BiMap can be defined with a sequence of keys and values
 + it is possible to get the value corresponding to a given key
 + it is possible to get the key corresponding to a given value
 + it is possible to know if a value exists
 + it is possible to know if a key exists
                                                                           """
}
trait BiMapExamples extends Grouped with MustMatchers {
  val bimap = Seq("one" <-> 1, "two" <-> 2, "three" <-> 3)

  "BiMap api" - new g1 {
    e1 := bimap.values === Seq(1, 2, 3)
    e2 := bimap.fromKey("one") === Some(1)
    e3 := bimap.fromValue(2) === Some("two")
    e4 := bimap.containsValue(3)
    e5 := bimap.containsKey("three")
  }
}
