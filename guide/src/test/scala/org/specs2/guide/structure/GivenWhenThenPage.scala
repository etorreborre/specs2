package org.specs2
package guide
package structure

import specification._
import script.{StepParser, StepParsers, StandardDelimitedStepParsers, GWT}
import scala.util.matching._

class GivenWhenThenPage extends UserGuidePage with GWT with StandardDelimitedStepParsers with SpecificationExecution with Snippets { def is = s2"""
### Given When Then

The Given/When/Then style of writing specifications is supported by the use of the `GWT` trait (there is also [another way](org.specs2.guide.structure.GivenWhenThenDeprecatedPage.html) but it is not recommended because it leads to performance issues at compile-time).

Here is a simple example of a Given/When/Then specification using the `GWT` trait: ${snippet{

class Calculator extends Specification with GWT with StandardDelimitedStepParsers { def is = s2"""

 Let's add numbers!

  the first number is {1}         ${addition.start}
  if I add it to {2}
  then I get {3}                  ${addition.end}
  """

  lazy val addition = Scenario("addition").
                        given(anInt).
                        when(anInt) { case i :: j :: _ => i + j }.
                        andThen(anInt) { case expected :: sum :: _ => sum === expected }

} // 8<---
executeSpec(new Calculator)
}.checkOk}

The first thing you can notice is that in specs2 you are not forced to use "Given"/"When"/"Then" as keywords. Those words are only guidelines to structure your scenario along the ["Arrange/Act/Assert"](http://c2.com/cgi/wiki?ArrangeActAssert) way of testing. Then you can see that the text contains some values delimited with `{}` and is annotated with the "start" and "end" of a `Scenario`, linking the text to some code. This is indeed the general pattern for writing GWT specifications:

1. write a piece of ***`s2`*** text containing with some values delimited by `{}`
2. define or reuse `StepParsers` to extract well-typed values from strings
3. create a `Scenario` object defining what each step should do: extract values only (`given`), execute actions (`when`), check results (`andThen`)

There are a few variations from this general pattern

#### Parsing

The `Scenario` requires `StepParser`s to extract strings from text and transform them into well-typed values.

##### Delimited parsers

Those parsers are using `{}` as delimiters for the values you want to extract. For example you can define an extractor for Int values like this: ${snippet{
  val anInt = StepParser((s: String) => s.toInt)
}}

You don't need to worry about catching exceptions, the `DelimitedStepParser` class (which is the type of object effectively created) will handle that for you.

If necessary you can change the delimiters by overriding the implicit regular expression from the `StepParsers` trait ${snippet{
// 8<--
trait NewStepParsers extends StepParsers { // 8<--
// use `[]` as a delimiter
override implicit lazy val stepParserRegex = new Regex("\\[([^\\]]+)\\]")
// 8<--
}
}}

##### Regex parsers

The other type of step parsers is `RegexStepParser`. Those parsers are using regular expressions with groups to extract values. You can create them with the `readAs` and `groupAs` methods ${snippet{

// match the whole line
val anInt1 = readAs(".*(\\d+).*").and((s: String) => s.toInt)
  
// just declare the group you want to match
val anInt2 = groupAs("\\d+").and((s: String) => s.toInt)

// note: if you want to extract 2 ints, just pass a function of 2 arguments
val twoInts = groupAs("\\d+").and((s1: String, s2: String) => (s1.toInt, s2.toInt))

}}

##### Standard parsers

A few `StepParsers` have been predefined for you in the `StandardDelimitedStepParsers` and `StandardRegexStepParsers` traits to extract `Int`s, `Double`s and `String`s

#### Scenario

A `Scenario` is a specific form of [`Script`](org.specs2.guide.Structure.html#Scripts) which declares in which order values must be extracted from the text and what to do with them. By default it uses a `ScriptTemplate` which is going to associate only the last lines of the text to the scenario steps (refer to the [`Script`](org.specs2.guide.Structure.html#Scripts) section of the User Guide to see how to change this).

The `Scenario` class provides 3 methods: `given`, `when`, `andThen` which can be passed `StepParsers` and functions:

 * `given` uses an extractor which will extract values for a line of text
 * `when` uses an extractor which will extract values from the corresponding line of text and a `mapping` function to combine the result of the extraction + all the values from the given steps
 * `andThen` uses an extractor (usually to extract expected values) and a `check` function taking `when values` and returning a `Result`

More precisely, the functions passed to a `when` step must be of the form

 * `when(aStepParser) { case p1 :: p2 :: .. :: _ => w1 }`, where `p1` has the type extracted from `aStepParser` and `p2 .. pn` have the types of the values extracted by the previous `given` steps
 * `when(aStepParser).collect { case (p1, p2n: Seq[LUB]) => w1 }`, where `p1` has the type extracted from `aStepParser` and `p2n` is a `Seq[LUB]` where `LUB` is the least upper bound of the types of all the values extracted by the previous `given` steps

`::` is the [Shapeless](https://github.com/milessabin/shapeless) `HCons` operator so don't forget at add the Shapeless dependency to your project if you are using the `GWT` trait!

And similarly for `andThen` steps

 * `andThen(aStepParser) { case p1 :: p2 :: .. :: _ => r: R }`, where `p1` has the type extracted from `aStepParser` and `p2 .. pn` have the types of the values mapped by the previous `when` steps
 * `andThen(aStepParser).collect { case (p1, p2n: Seq[LUB]) => r: R }`, where `p1` has the type extracted from `aStepParser` and `p2n` is a `Seq[LUB]` where `LUB` is the least upper bound of the types of all the values mapped by the previous `when` steps

The type `R` of the value `r` must be such that there is an `AsResult` type class instance in scope to transform it to a `Result`. In other words r is: a `Boolean`, a `MatchResult[_]`, a ScalaCheck `Prop`,...

You will also note that the `Scenario` class restricts the order of methods which you can call. It always has to be `given* -> when* -> andThen*`

#### Tagging

When you insert a `Scenario.start` and `Scenario.end` in a piece of `s2` text, this will automatically create a tagged "`Section`" with the title of the Scenario so that you can execute only this scenario from the command-line by running:
```
sbt> test-only *Calculator* -- include addition
```
""" ^
  include(xonly, new GivenWhenThenDeprecatedPage)
}