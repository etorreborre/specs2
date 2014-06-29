package org.specs2
package specification
package dsl
package mutable

class GivenWhenThenSpec extends org.specs2.mutable.Specification { //with GivenWhenThen {
  "a class with the GivenWhenThen trait".txt.p

  "given this".p
  step("pp")

  "when that".br
  step("else")

  "result" >> ok

}
