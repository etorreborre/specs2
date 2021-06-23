package org.specs2
package guide
package matchers

object MapMatchers extends UserGuideCard {
  def title = "Map"
  def text = s2"""
Maps have their own matchers as well, to check keys and values:

 * `haveKey` checks if a Map has a given key
   `Map(1 -> "1") must haveKey(1)`

 * `haveKeys` checks if a Map has several keys
   `Map(1 -> "1", 2 -> "2") must haveKeys(1, 2)`

 * `haveValue` checks if a Map has a given value
   `Map(1 -> "1") must haveValue("1")`

 * `haveValues` checks if a Map has several values
   `Map(1 -> "1", 2 -> "2") must haveValues("1", "2")`

 * `havePair` checks if a Map has a given pair of values
   `Map(1 -> "1") must havePair(1 -> "1")`

 * `havePairs` checks if a Map has some pairs of values
   `Map(1->"1", 2->"2", 3->"3") must havePairs(1->"1", 2->"2")`

 But Maps are also PartialFunctions, so:

 * `beDefinedAt` checks if a PartialFunction is defined for a given value
   `partial must beDefinedAt(1)`

 * `beDefinedBy` checks if a PartialFunction is defined for a given value
 and returns another one
   `partial must beDefinedBy(1 -> true)`
"""
}
