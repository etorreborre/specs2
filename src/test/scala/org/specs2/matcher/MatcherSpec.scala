package org.specs2
package matcher

class MatcherSpec extends SpecificationWithJUnit {
  def is = 
"  a matcher can be adapted with a function"              ! e1^
                                                          end

  def e1 = new Exception("message")  must be_==("message") ^^ ((_:Exception).getMessage)
}