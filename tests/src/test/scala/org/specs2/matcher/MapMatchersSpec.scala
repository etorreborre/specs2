package org.specs2
package matcher

class MapMatchersSpec extends Spec with MapMatchers { def is = s2"""

 The MapMatchers trait provides matchers to check Map instances.

 haveKey checks if a Map has a given key
 ${ Map(1 -> "1") must haveKey(1) }
 ${ Map(1 -> "1") must not have key(2) }
 ${ Map(1 -> "1") must not haveKey(2) }

 haveKeys checks if a Map has several keys
 ${ Map(1 -> "1", 2 -> "2") must haveKeys(1, 2) }
 ${ (Map(1 -> "1", 2 -> "2") must haveKeys(1, 3)) returns "doesn't have the key '3'" }
 ${ (Map(1 -> "1", 2 -> "2") must not haveKeys(2, 3)) returns "has the key '2'" }

 haveValue checks if a Map has a given value
 ${ Map(1 -> "1") must haveValue("1") }
 ${ Map(1 -> "1") must not have value("2") }
 ${ Map(1 -> "1") must not haveValue("2") }

 haveValues checks if a Map has several values
 ${ Map(1 -> "1", 2 -> "2") must haveValues("1", "2") }
 ${ (Map(1 -> "1", 2 -> "2") must haveValues("1", "3")) returns "doesn't have the value '3'" }

 havePair checks if a Map has a given pair of values
 ${ Map(1 -> "1") must havePair(1 -> "1") }
 ${ Map(1 -> "1") must not have pair(1 -> "2") }
 ${ Map(1 -> "1") must not havePair(1 -> "2") }

 havePairs checks if a Map has some pairs of values
 ${ Map(1->"1", 2->"2", 3->"3") must havePairs(1->"1", 2->"2") }
 ${ Map(1->"1", 2->"2", 3->"3") must not have pairs(1->"2", 2->"1") }
 ${ Map(1->"1", 2->"2", 3->"3") must not havePairs(1->"2", 2->"1") }

 beDefinedAt checks if a PartialFunction is defined for a given value
 ${ partial must beDefinedAt(1) }
 ${ partial must not be definedAt(2) }
 ${ partial must not beDefinedAt(2) }

 beDefinedBy checks if a PartialFunction is defined for a given value
 and returns another one"
 ${ partial2 must beDefinedBy(1 -> true) }
 ${ partial must not be definedBy(1 -> false) }
 ${ partial must not beDefinedBy(1 -> false) }
                                                                                                                        """
                                                                                          
  val partial: PartialFunction[Int, Boolean] = { case (i: Int) if (i == 1) => true }                                                                                        
  val partial2: PartialFunction[Int, Boolean] = { case (i: Int) => (i == 1) }                                                                                        
}