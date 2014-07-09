package org.specs2
package guide
package matchers

import form.Card

object MapMatchers extends UserGuideCard {
  def title = "Map"
  def text = s2"""
Maps have their own matchers as well, to check keys and values:

 * `haveKey` check if a Map has a given key
   `Map(1 -> "1") must haveKey(1)`

 * `haveKeys` check if a Map has several keys
   `Map(1 -> "1", 2 -> "2") must haveKeys(1, 2)`

 * `haveValue` check if a Map has a given value
   `Map(1 -> "1") must haveValue("1")`

 * `haveValues` check if a Map has several values
   `Map(1 -> "1", 2 -> "2") must haveValue("1", "2")`

 * `havePair` check if a Map has a given pair of values
   `Map(1 -> "1") must havePair(1 -> "1")`

 * `havePairs` check if a Map has some pairs of values
   `Map(1->"1", 2->"2", 3->"3") must havePairs(1->"1", 2->"2")`

 But Maps are also Partial Functions, so:

 * `beDefinedAt` check if a PartialFunction is defined for a given value
   `partial must beDefinedAt(1)`

 * `beDefinedBy` check if a PartialFunction is defined for a given value
 and returns another one
   `partial must beDefinedBy(1 -> true)`
"""
}
