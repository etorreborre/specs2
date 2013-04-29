package org.specs2
package guide
package structure

import specification._
import shapeless.::

class GivenWhenThenPage extends UserGuidePage with GivenWhenThenPageImplementation { def is = s2"""
### Given When Then

The Given/When/Then style of writing specifications is supported by the use of the `GWT` trait (there is also [another way](org.specs2.guide.structure.GivenWhenThenDeprecatedPage.html) but it is not recommended because it leads to performance issues at compile-time).

Here is a simple example of a Given/When/Then specification using the `StepParsers` trait: ${ snippet {

class example extends Specification with GWT { def is = s2"""
                                  ${addition.start}
  Given a number {1}
  If I add another number {2}
  Then I get {3}                  ${addition.end}
                                  """

  val addition = GWTSteps("addition").
      given(anInt).
      when(anInt) { case i :: j :: _ => i + j }.
      andThen(anInt) { case expected :: sum :: _ => sum === expected }

  def anInt    = StepParser((_: String).toInt)

} // 8<---
executeSpec(new example)
}.checkOk}

The first thing you can notice is that in specs2 you are not forced to use "Given"/"When"/"Then" as keywords. Those words are only guidelines to structure your scenario along the "Arrange/Act/Assert" way of testing. Actually *how* values are extracted and *how* results are checked entirely depends on the `StepParsers` which you insert after the pieces of text.

Let's dissect the specification above. First we import the `StepParsers` trait which gives us access to the `extract` method. The `extract` method extracts String values from the preceding text. It finds them by looking at strings enclosed by `{}` (we'll see how to avoid using delimiters later). Then it applies a function, from `String` to `Int` to create well-typed "Given" values or "When" actions.

The values `a1` and `a2` above effectively serve as variables which will hold `Int` values once the text is parsed. Those variable can then be used, with other variables, to create "Then" verifications like `result`. Indeed, on the line `Then I get...`, `result` parses the preceding text to extract `3` and uses this value to create an expectation `a1 + a2 === r` so that the whole line is finally transformed to an `Example` fragment in the final Specification.

From this general pattern we can derive a few variations

#### Parsing

##### Constructor

The `extract` method can be used to create a `FragmentParser` with a function but you can also be more explicit and use the `FragmentParser` constructor:

```
val a1 = FragmentParser((_: String).toInt)
```

##### Parameters

When you use delimiters it is possible to extract several values at once, simply by passing a function which will use all of them:
// 8<----
def anInt   = extract[Int]((_:String).toInt)
// 8<----
def twoInts = extract[(Int, Int)]((s1: String, s2: String) => (s1.toInt, s2.toInt))
// 8<----
val result  = anInt((r: Int) => twoInts._1 + twoInts._2 === r)
// 8<----
                                  s2""
Adding 2 numbers {1} and {2}      twoInts
Should return {3}                 result
                                  ""

You can also pass a function taking in `Seq[String]` as a parameter and get all the extracted values as a sequence.

Note that you will get a runtime exception if you provide a function having more parameters than extracted values (on the other hand if you pass a function having less parameters than extracted values it will only use the first ones).

##### Default delimiters

The `{}` delimiters are being used as the default delimiters but you can change them via another regular expression if you prefer:
// you need to define a group that will capture values
implicit val fragmentParserRegex = ""\[([^\]]+)\]"".r

def anInt   = extract[Int]((_:String).toInt)
def twoInts = extract[(Int, Int)]((s1: String, s2: String) => (s1.toInt, s2.toInt))
val result  = anInt((r: Int) => twoInts._1 + twoInts._2 === r)

                                s2""
The maximum of [1] and [2]      twoInts
Should return [3]               result
                                ""


                                                                                  """ ^
  include(xonly, new GivenWhenThenDeprecatedPage)
}

trait GivenWhenThenPageImplementation extends GWT with SpecificationExecution with Snippets { this: Specification =>

}