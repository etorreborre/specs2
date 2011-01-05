package org.specs2
package matcher

class MatcherSpec extends SpecificationWithJUnit { def is =
                                                                                                         """
  Matchers can be created in different ways
                                                                                                         """^
                                                                                                         p^
  "a matcher can be adapted with a function"                                                             ! e1^
  "a matcher can be adapted with a function for both expected and actual values"                         ! e2^
  "a matcher can be defined by a function"                                                               ! e3^
  "the name of a matcher is build from its class name"                                                   ! name1^
  "the name of a matcher can be defined if it is derived from another one, like beGreaterThan"           ! name2^
                                                                                                          end

  def e1 = new Exception("message")  must be_==("message") ^^ ((_:Exception).getMessage)
  def e2 = {
    case class Human(age: Int, wealth: Int)
    def beMostlyEqualTo = (be_==(_:Human)) ^^^ ((_:Human).copy(wealth = 0))
    Human(age = 20, wealth=1000) must beMostlyEqualTo(Human(age = 20, wealth=1)) toResult
  }
  def e3 = {
    def beEven: Matcher[Int] = (i: Int) => (i % 2 == 0, i + " is even", i + " is not even")
    2 must beEven
  }
  def name1 = beLessThanOrEqualTo(3).name must_== "be less than or equal to"
  def name2 = beGreaterThanOrEqualTo(3).name must_== "be greater than or equal to"

}