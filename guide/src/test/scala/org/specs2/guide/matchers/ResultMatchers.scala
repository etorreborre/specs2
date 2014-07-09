package org.specs2
package guide
package matchers

import form.Card

object ResultMatchers extends UserGuideCard {
  def title = "Result"
  def text =  s2"""
That's only if you want to match the result of other matchers! ${snippet{

// you need to extend the ResultMatchers trait
class MatchersSpec extends Specification with matcher.ResultMatchers { def is =
  "beMatching is using a regexp" ! {
    ("Hello" must beMatching("h.*")) must beSuccessful
  }
}
}}
"""
}
