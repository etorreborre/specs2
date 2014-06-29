package org.specs2
package guide

import specification.dsl

object GivenWhenThenStyle extends UserGuidePage { def is = s2"""

## Presentation

The Given/When/Then style structures the specification with 3 main elements:

 - Given steps: actions which setup the system
 - When steps:  command which your are currently specifying
 - Then steps:  expectations about the end state of the system

In specs2 the support for Given/When/Then can be more or less complex depending on the features you with to use:

 - basic support: given and when steps are just commands on a system, returning no value
 - intermediate support: given/when/then steps can parse part of the text to create their commands
 - full support: variables are not necessary to save intermediate values and the given/when/then order is enforced

## Basic support

### With an acceptance specification

Given/When/Then specifications are easy to write using the acceptance style, you just use regular text, steps and examples:${snippet{
class GWTSpec extends Specification { def is = s2"""
 Given a first number         $g1
 When I double it             $w1
 Then I get twice that number $t1
"""
  var number = 0
  def g1 = step {
    // do something to provide a number
    number = 1
  }

  def w1 = step {
    // do an action
    number *= number
  }

  def t1 = number must_== 2
}
}}

### With a mutable specification

With a mutable specification you would write:${snippet{
class GWTSpec extends mutable.Specification {

 "Given a first number".txt.p
  step { number = 1 }

  "When I double it".br
  step { number *= number }

  "Then I get twice that number" >> {
    number must_== 2
  }

  var number = 0
}
}}

## Intermediate support

Given/When/Then specifications are often written as block of texts where part of the text contains values to use during the execution. For example

This is a specification for a bank transfer

 given an account with the amount {100}
 given another account with the amount {500}
 when we transfer {50} from the first account to the second
 then the first amount is {50} and the second is {550}

How do you code this with an acceptance specification?

### With an acceptance specification

You can implement this approach with the `org.specs2.specification.dsl.GivenWhenThen` trait:${snippet{
class GWTSpec extends Specification with dsl.GivenWhenThen { def is = s2"""
 Given a first number         $g1
 When I double it             $w1
 Then I get twice that number $t1
"""
    var number = 0
    def g1 = step {
      // do something to provide a number
      number = 1
    }

    def w1 = step {
      // do an action
      number *= number
    }

    def t1 = number must_== 2
  }
}}

### With a mutable specification

With a mutable specification you would write:${snippet{
  class GWTSpec extends mutable.Specification {

    "Given a first number".txt.p
    step { number = 1 }

    "When I double it".br
    step { number *= number }

    "Then I get twice that number" >> {
      number must_== 2
    }

    var number = 0
  }
}}

The `org.specs2.specification.dsl.mutable.GivenWhenThen` trait

"""
}
