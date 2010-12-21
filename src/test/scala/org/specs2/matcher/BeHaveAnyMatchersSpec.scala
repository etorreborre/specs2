package org.specs2
package matcher

class BeHaveAnyMatchersSpec extends SpecificationWithJUnit { def is = 
  
  "The expressions are allowed"                                                           ^
    "using and and be in combination"                                                     ^
    { 1 must be equalTo(1)  }                                                             ^
                                                                                          end
}