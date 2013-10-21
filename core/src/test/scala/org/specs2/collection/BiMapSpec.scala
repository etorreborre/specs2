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

trait BiMapExamples extends Groups with SpecificationLike {
  val bimap = Seq("one" <-> 1, "two" <-> 2, "three" <-> 3)

  "BiMap api" - new group {
    eg := bimap.values === Seq(1, 2, 3)
    eg := bimap.fromKey("one") === Some(1)
    eg := bimap.fromValue(2) === Some("two")
    eg := bimap.containsValue(3)
    eg := bimap.containsKey("three")
  }
}
