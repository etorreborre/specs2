package org.specs2
package specification

import matcher.ResultMatchers
import main.Arguments

class FragmentParsersSpec extends Specification with RegexFragmentParsers with SpecificationExecution with Snippets { def is =
"Fragment parsers".title ^ s2"""

It is possible to write a specification in the Given/When/Then format by using the `FragmentParsers` trait. For example: ${ snippet {

  // 8<---
  class example extends Specification with FragmentParsers { def is = s2"""

    Given a number {1}              $a1
    If I add another number {2}     $a2
    Then I get {3}                  $result
                                    """

    def anInt    = extract[Int]((_: String).toInt)
    val (a1, a2) = (anInt, anInt)
    val result   = anInt((r: Int) => a1 + a2 === r)
  }
  // 8<---
  executeSpec(new example)
}.check(r => r must beSuccessful)}

Let's dissect the specification above. First we import the `FragmentParsers` trait which gives us access to the `extract` method. The `extract` method extracts String values from the preceding text. It finds them by looking at strings enclosed by `{}` (we'll see how to avoid using delimiters later). Then it applies a function, from `String` to `Int` to create the "Given" values or "When" actions.

Effectively the values `a1` and `a2` above serve as a variables which will hold `Int` values once the text is parsed. Those variable can then be used, with other variables, to create "Then" verifications like `result`. For instance, on the line `Then I get...`, `result` parses the preceding text to extract `3` and uses this value to create an expectation `a1 + a2 === r` so that the whole line is finally transformed to an `Example` fragment in the final Specification.

We can now see a few variations on this general pattern:

#### Parsing

##### Delimiters

When you use delimiters it is possible to extract several values at once, simply by passing a function which will use all of them: ${ snippet {
  def anInt   = extract[Int]((_:String).toInt)                                           // 8<----
  def twoInts = extract[(Int, Int)]((s1: String, s2: String) => (s1.toInt, s2.toInt))  // 8<----
  val result  = anInt((r: Int) => twoInts._1 + twoInts._2 === r)                        // 8<----

  s2"""
    Adding 2 numbers {1} and {2}      $twoInts
    Should return {3}                 $result
  """
                                                                                       // 8<----
}}

##### Default

The `{}` delimiters are being used as the default delimiters but you can change them via another regular expression if you prefer: ${ snippet {
  // you need to define a group that will capture values
  implicit val fragmentParserRegex = """\[([^\]]+)\]""".r

  def anInt   = extract[Int]((_:String).toInt)
  def twoInts = extract[(Int, Int)]((s1: String, s2: String) => (s1.toInt, s2.toInt))
  val result  = anInt((r: Int) => twoInts._1 + twoInts._2 === r)

  s2"""
    The maximum of [1] and [2]      $twoInts
    Should return [3]               $result
  """
}}

 * import

 * Write sentences where the variables to extract are delimited with specific markers
 * Write sentences where the variables to extract and the


                                                                                  """
}

/**
 * execute a Specification and check the results
 */
trait SpecificationExecution extends ResultMatchers { this: Specification =>
  def executeSpec(s: SpecificationStructure)       = FragmentExecution.executeExamplesResult(s.content)(Arguments())
  def executionMustBeOk(s: SpecificationStructure) = executeSpec(s) must beSuccessful
}
