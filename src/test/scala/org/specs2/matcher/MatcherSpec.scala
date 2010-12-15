package org.specs2
package matcher

class MatcherSpec extends SpecificationWithJUnit { def is =

  "a matcher can be adapted with a function"                                            ! e1^
  "a matcher can be defined by a function"                                              ! e2^
                                                                                         end

  def e1 = new Exception("message")  must be_==("message") ^^ ((_:Exception).getMessage)
  def e2 = {
    def beEven: Matcher[Int] = (i: Int) => (i % 2 == 0, i + " is even", i + " is not even")
    2 must beEven
  }
}