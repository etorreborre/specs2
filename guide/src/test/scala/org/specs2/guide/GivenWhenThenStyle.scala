package org.specs2
package guide

import specification.script.{StandardRegexStepParsers, StepParsers, StepParser, StandardDelimitedStepParsers}
import specification.dsl.mutable.{GivenWhenAndThenSyntax, GivenWhenThenSyntax}
import scala.util.matching.Regex

object GivenWhenThenStyle extends UserGuidePage { def is = "Given when then".title ^ s2"""

## Presentation

The Given/When/Then style structures the specification with 3 main elements:

 - Given steps: actions which setup the system
 - When steps:  command which your are currently specifying
 - Then steps:  expectations about the end state of the system

In $specs2 the support for Given/When/Then can be more or less complex depending on the features you wish to use:

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
  // check the result
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

Given/When/Then specifications are often written as blocks of text where part of the text contains values to use during the execution. For example:
```
This is a specification for a bank transfer

 given an account with the amount {100}
 given another account with the amount {500}
 when we transfer {50} from the first account to the second
 then the first amount is {50} and the second is {550}
```
How do you code this with an acceptance specification?

### With an acceptance specification

You can implement this approach with the `org.specs2.specification.dsl.GWT` trait:${snippet{
class GWTSpec extends Specification with specification.dsl.GWT with StandardDelimitedStepParsers { def is = s2"""
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

The easiest way to extract values from a string is to delimit exactly where are the values to extract, then to provide a way to give them meaningful types. This is the role of `StepParsers`. Those parsers are using `{}` as delimiters for the values you want to extract. For example you can define an extractor for Int values like this: ${snippet{
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

Finally `StepParsers` can collect all the delimited values at once with the `seq` method:${snippet{
  StepParser.seq((seq: Seq[String]) => seq.map(_.toInt).sum).parse("values {1}, {2}, {3}") === Right(6)
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
class GWTSpec extends mutable.Specification with org.specs2.specification.dsl.mutable.GWT with StandardDelimitedStepParsers {

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
class GWTSpec extends mutable.Specification with org.specs2.specification.dsl.mutable.GWT with StandardDelimitedStepParsers {

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

Those two syntaxes are just specialisations of the `mutable.GivenWhenThen` trait which provides  `Given/When/Then` keywords:${snippet{
class GWTSpec extends mutable.Specification with org.specs2.specification.dsl.mutable.GWT with StandardDelimitedStepParsers with GivenWhenThenSyntax {

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

If you prefer to have uncapitalized `given/when/then` methods you can use the `GivenWhenAndThenSyntax` trait:${snippet{
class GWTSpec extends mutable.Specification with org.specs2.specification.dsl.mutable.GWT
  with StandardDelimitedStepParsers with GivenWhenAndThenSyntax {

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

In this case `andThen` has to be used in place of `then` because `then` is going to become a Scala keyword in future releases.

## Full support

The full support fixes an issue with all the previous styles: the necessity to create mutable variables to keep track of state changes between steps and examples. It also enforces statically a proper sequencing of the Given/When/Then actions.

Here, we mix-in the `org.specs2.specification.script.GWT` trait. This trait provides a class, `Scenario`, to parse the specification text and create `Steps` and `Examples`. Let's see an example:${snippet{
class GWTSpec extends Specification with org.specs2.specification.script.GWT with StandardRegexStepParsers { def is = s2"""

 A given-when-then example for a calculator                       ${calculator1.start}
   Given the following number: 1
   And a second number: 2
   And a third number: 6
   When I use this operator: +
   Then I should get: 9
   And it should be >: 0                                          ${calculator1.end}

"""

  val anOperator = readAs(".*: (.)$").and((s: String) => s)

  val calculator1 =
    Scenario("calculator1").
      given(anInt).
      given(anInt).
      given(anInt).
      when(anOperator) { case op :: i :: j :: k :: _ => if (op == "+") i + j + k else i * j * k }.
      andThen(anInt)   { case expected :: sum :: _ => sum === expected}.
      andThen(anInt)   { case expected :: sum :: _ => sum must be_>(expected)}
}
}}

In this example, `calculator1.start` marks the beginning of a Given/When/Then section and each line until `calculator1.end` must correspond to a `given`, `when` or `andThen` call on the scenario.

 * `given` uses a `StepParser` to extract values for a line of text
 * `when` uses a `StepParser` to extract values from the corresponding line of text and a `mapping` function to combine the result of the extraction + all the values from the given steps
 * `andThen` uses a `StepParser` (usually to extract expected values) and a `check` function taking `when values` and returning a `Result`

More precisely, the functions passed to a `when` step must be of the form

 * `when(aStepParser) { case p1 :: p2 :: .. :: _ => w1 }`, where `p1` has the type extracted from `aStepParser` and `p2 .. pn` have the types of the values extracted by the previous `given` steps
 * `when(aStepParser).collect { case (p1, p2n: Seq[LUB]) => w1 }`, where `p1` has the type extracted from `aStepParser` and `p2n` is a `Seq[LUB]` where `LUB` is the least upper bound of the types of all the values extracted by the previous `given` steps

`::` is the [Shapeless](https://github.com/milessabin/shapeless) `HCons` operator so don't forget to add the Shapeless dependency to your project if you are using the `GWT` trait!

And similarly for `andThen` steps

 * `andThen(aStepParser) { case p1 :: p2 :: .. :: _ => r: R }`, where `p1` has the type extracted from `aStepParser` and `p2 .. pn` have the types of the values mapped by the previous `when` steps
 * `andThen(aStepParser).collect { case (p1, p2n: Seq[LUB]) => r: R }`, where `p1` has the type extracted from `aStepParser` and `p2n` is a `Seq[LUB]` where `LUB` is the least upper bound of the types of all the values mapped by the previous `when` steps

The type `R` of the value `r` must be such that there is an `AsResult` type class instance in scope to transform it to a `Result`. In other words r is: a `Boolean`, a `MatchResult[_]`, a ScalaCheck `Prop`,...

You will also note that the `Scenario` class restricts the order of methods which you can call. It always has to be `given* -> when* -> andThen*`.

$AndIfYouWantToKnowMore

 - read about the ${"dependencies for the GWT trait" ~/ RunInShell}

$vid


"""
}
