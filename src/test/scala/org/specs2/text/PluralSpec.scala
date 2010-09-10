package org.specs2
package text

class PluralSpec extends Specification with Plural {
  val examples = 
"""A string can be pluralized: "apple".plural(n) """^
    "if the quantity is 0, no 's' must be added" ! e1^
    "if the quantity is 0, no 's' must be added" ! e2^
    "if the quantity is 2 or more, 's' must added to the string" ! e3^
    "also with a long value" ! e4
    
  def e1 = "apple".plural(0) must_== "apple"
  def e2 = "apple".plural(1) must_== "apple"
  def e3 = "apple".plural(2) must_== "apples"
  def e4 = "apple".plural(2L) must_== "apples"
}