package org.specs2
package collection

import BiMap.*

class BiMapSpec extends Specification with ScalaCheck { def is = s2"""

Bimaps define bijection relationships between values

  a BiMap can be defined with a sequence of keys and values $bimap1
  it is possible to get the value corresponding to a given key $bimap2
  it is possible to get the key corresponding to a given value $bimap3
  it is possible to know if a value exists $bimap4
  it is possible to know if a key exists $bimap5
  all keys and values must be in the map $bimap6

"""

  val bimap = BiMap.fromSeq("one" <-> 1, "two" <-> 2, "three" <-> 3)

  def bimap1 = bimap.values === Seq(1, 2, 3)
  def bimap2 = bimap.fromKey("one") === Some(1)
  def bimap3 = bimap.fromValue(2) === Some("two")
  def bimap4 = bimap.containsValue(3)
  def bimap5 = bimap.containsKey("three")

  def bimap6 = prop { (keys: List[String], values: List[Int]) =>
    val zipped = keys.distinct.zip(values.distinct)
    val bimap: BiMap[String, Int] = BiMap.fromSeq(zipped.map { case (k, v) => k <-> v }*)

    zipped must contain { (kv: (String, Int)) => kv match {
      case (k, v) =>
        (bimap.fromKey(k) must beSome(v)) `and`
        (bimap.fromValue(v) must beSome(k))
    }}.forall
  }

}
