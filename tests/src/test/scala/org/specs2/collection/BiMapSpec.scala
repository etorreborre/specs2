package org.specs2
package collection

import BiMap._
import specification._

class BiMapSpec extends script.Specification with Groups with ScalaCheck { def is = s2"""

 Bimaps define bijection relationships between values

 + a BiMap can be defined with a sequence of keys and values
 + it is possible to get the value corresponding to a given key
 + it is possible to get the key corresponding to a given value
 + it is possible to know if a value exists
 + it is possible to know if a key exists

 Properties
 ==========

 + all keys and values must be in the map
 """

  "BiMap api" - new group {
    val bimap = Seq("one" <-> 1, "two" <-> 2, "three" <-> 3)

    eg := bimap.values === Seq(1, 2, 3)
    eg := bimap.fromKey("one") === Some(1)
    eg := bimap.fromValue(2) === Some("two")
    eg := bimap.containsValue(3)
    eg := bimap.containsKey("three")
  }

  "properties" - new group {
    eg := prop { (keys: List[String], values: List[Int]) =>
      val zipped = keys.distinct.zip(values.distinct)
      val bimap: BiMap[String, Int] = zipped.map { case (k, v) => k <-> v }

      zipped must contain { kv: (String, Int) => kv match {
        case (k, v) =>
          (bimap.fromKey(k) must beSome(v)) and
          (bimap.fromValue(v) must beSome(k))
      }}.forall
    }
  }

}
