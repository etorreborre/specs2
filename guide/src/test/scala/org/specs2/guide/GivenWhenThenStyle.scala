package org.specs2
package guide

import org.specs2.specification.dsl.mutable.{GivenWhenAndThenSyntax, GivenWhenThenSyntax}
import org.specs2.specification.script.{StepParsers, StepParser, StandardDelimitedStepParsers}
import specification.dsl

import scala.util.matching.Regex

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

Given/When/Then specifications are often written as block of texts where part of the text contains values to use during the execution. For example:
```
This is a specification for a bank transfer

 given an account with the amount {100}
 given another account with the amount {500}
 when we transfer {50} from the first account to the second
 then the first amount is {50} and the second is {550}
```
How do you code this with an acceptance specification?

### With an acceptance specification

You can implement this approach with the `org.specs2.specification.dsl.GivenWhenThen` trait:${snippet{
class GWTSpec extends Specification with dsl.GivenWhenThen with StandardDelimitedStepParsers{ def is = s2"""
 Given a first number {2}     $g1
 When multiply it by {3}      $w1
 Then I get {6}               $t1
"""
  var number = 0
  def g1 = step(anInt) { i => number = i }

  def w1 = step(anInt) { j => number = number * j }

  def t1 = example(anInt) { n => number must_== n }
}
}}

Now we need a bit of help to extract values from the text. This is provided in the form of `StepParsers`.

#### Delimited parsers

The easiest way to extract values from a string is to delimit exactly where are the values to extract, then to provide a way to transform those values to meaningful types. This is the role of `StepParsers`. Those parsers are using `{}` as delimiters for the values you want to extract. For example you can define an extractor for Int values like this: ${snippet{
  val anInt = StepParser((_: String).toInt)
}}

The `anInt` step parser has a method `parse(text: String)` returning:

 - either an exception if the parse is unsuccessful
 - or the desired value + the original text stripped for its delimiters

You can change the delimiters being used by overriding the implicit regular expression from the `StepParsers` trait: ${snippet{
// 8<--
trait NewStepParsers extends StepParsers { // 8<--
  // use `[]` as a delimiter
  override implicit lazy val stepParserRegex = new Regex("\\[([^\\]]+)\\]")
// 8<--
}
}}

But you can also specify another regular expression "locally" for a given step parser:${snippet{
  val anInt = StepParser((_: String).toInt)(new Regex("\\[([^\\]]+)\\]"))
}}

#### Regex parsers

More generally you can use any regular expression to parse values with the `readAs` and `groupAs` methods ${snippet{
// 8<---
import StepParsers._
// 8<---
// match the whole line
val anInt1 = readAs(".*(\\d+).*").and((s: String) => s.toInt)

// just declare the groups you want to match
val anInt2 = groupAs("\\d+").and((s: String) => s.toInt)

// note: if you want to extract 2 ints, just pass a function of 2 arguments
val twoInts = groupAs("\\d+").and((s1: String, s2: String) => (s1.toInt, s2.toInt))

}}

#### Standard parsers

A few `StepParsers` have been predefined for you in the `StandardDelimitedStepParsers` and `StandardRegexStepParsers` traits to extract `Int`s, `Double`s and `String`s:

 - `anInt`, `twoInts`, `threeInts`
 - `aDouble`, `twoDoubles`, `threeDoubles`
 - `aString`, `twoStrings`, `threeStrings`

### With a mutable specification

Several syntaxes are available with a mutable specification. The first syntax uses modified `step` and `example` methods to create steps and examples:${snippet{
class GWTSpec extends mutable.Specification with dsl.mutable.GivenWhenThen with StandardDelimitedStepParsers {

  "adding numbers".p

  step("Given a first number {2}")(anInt) { i =>
    number = i
  }

  step("When I multiply it by {3}")(anInt) { j =>
    number = number * j
  }

  example("Then I get {6}")(anInt) { n: Int =>
    number must_== n
  }

  var number = 0
}
}}

The second syntax is mostly the same but with postfix methods:${snippet{
class GWTSpec extends mutable.Specification with dsl.mutable.GivenWhenThen with StandardDelimitedStepParsers {

  "adding numbers".p

  "Given a first number {2}".step(anInt) { i =>
    number = i
  }

  "When I multiply it by {3}".step(anInt) { j =>
    number = number * j
  }

  "Then I get {6}".example(anInt) { n: Int =>
    number must_== n
  }

  var number = 0
}
}}

The final 2 syntaxes are just specialisations of the `mutable.GivenWhenThen` trait with the `Given/When/Then` keywords:${snippet{
class GWTSpec extends mutable.Specification with dsl.mutable.GivenWhenThen with StandardDelimitedStepParsers with GivenWhenThenSyntax {

  "adding numbers".p

  Given("a first number {2}")(anInt) { i =>
    number = i
  }

  When("I multiply it by {3}")(anInt) { j =>
    number = number * j
  }

  Then("I get {6}")(anInt) { n: Int =>
    number must_== n
  }

  var number = 0
}
}}

This will create sentences such as:
```
Given a first number 2
When I multiply it by 3
Then I get 6
```

If you prefer to have uncapitalized `given/when/then` you can use the `GivenWhenAndThenSyntax`:${snippet{
  class GWTSpec extends mutable.Specification with dsl.mutable.GivenWhenThen with StandardDelimitedStepParsers with GivenWhenAndThenSyntax {

    "adding numbers".p

    given("a first number {2}")(anInt) { i =>
      number = i
    }

    when("I multiply it by {3}")(anInt) { j =>
      number = number * j
    }

    andThen("I get {6}")(anInt) { n: Int =>
      number must_== n
    }

    var number = 0
  }
}}

Which renders
```
given a first number 2
when I multiply it by 3
then I get 6
```

In this case `andThen` has to be used in place of `then` because `then` is going to become a Scala keyword.


"""
}
