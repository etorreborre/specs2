package org.specs2
package matcher

class OptionMatchersSpec extends Specification with ResultMatchers { def is = s2"""

 The OptionMatchers trait provides matchers to check Option instances.

  beSome checks if an element is Some(_)
  ${ Some(1) must beSome }
  ${ Some(1) must beSome(1) }
  ${ Some(1) must beSome.which(_ > 0) }
  ${ ((None: Option[Int]) must beSome.which(_ > 0)) returns "'None' is not Some" }
  ${ ((None: Option[Int]) must beSome.like { case i => i must be_>(0) }) returns "'None' is not Some" }
  ${ (Some(1) must beSome.which(_ > 0)) returns "'Some(1)' is Some and the function returns 'true' on '1'" }
  ${ (Some(1) must beSome.which(_ < 0)) returns "'Some(1)' is Some but the function returns 'false' on '1'" }
  ${ Some(1) must beSome.like { case a if a > 0 => ok } }
  ${ (Some(1) must not(beSome.like { case a => a must be_>=(0) })) returns "'Some(1)' is Some and 1 is not less than 0" }
  ${ Some(1) must not be some(2) }
  ${ None must not be some }
  ${ None must not be some(2) }

  beNone checks if an element is None
  ${ None must beNone }
  ${ Some(1) must not be none }

  beAsNoneAs checks if 2 values are None at the same time
  ${ None must beAsNoneAs(None) }
  ${ Some(1) must beAsNoneAs(Some(2)) }
  """
}
