package org.specs2
package examples

class GivenWhenThenSpec extends SpecificationWithJUnit { def is = noindent^
                                                                                                   """
  This specification shows how to write examples in a Given-When-Then style
                                                                                                   """^
//   "Given an employee named Bob making $12 per hour."                                              ! e1^
//   "When Bob works 40 hours in one week"                                                           ! e2^
//   "Then Bob will be paid $480 on Friday evening"                                                  ! e3^
                                                                                                   end

  def e1 = (s: String) => {
    success
  }
  def e2 = (s: String) => {
    success
  }
  def e3 = (s: String) => {
    success
  }
}