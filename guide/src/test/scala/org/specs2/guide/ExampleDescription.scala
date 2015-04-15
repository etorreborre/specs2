package org.specs2
package guide

import matcher._
import org.specs2.specification.script.{StandardDelimitedStepParsers, StepParsers}
import StepParsers._
import StandardDelimitedStepParsers._

object ExampleDescription extends UserGuidePage { def is = s2"""

["Don't repeat yourself"](http://en.wikipedia.org/wiki/Don%27t_repeat_yourself) mandates that the same information is not repeated twice.
However this situation happens when part of an example description is reused in the example body: ${snippet{
s2"""
  1971-01-01 is a correct date $correct
"""

  def correct = { "1971-01-01" must beCorrect }
}}

### Use the example description

You can avoid this by creating the example body as a function using the description string: ${snippet{
s2"""
 1971-01-01 is a correct date $correct
"""
  def correct = { date: String =>
    date.split(" ")(0) must beCorrect
  }
}}

### Parse the example description

#### Delimited values

We can reuse the `StepParsers` presented in the ${"Given/When/Then style" ~/ GivenWhenThenStyle } to extract the values we wish to use: ${snippet{
s2"""
 {1971-01-01} is a correct date $correct
 {1} plus {1} is {2}            $addition
"""

  import org.specs2.specification.script.StepParsers._

  def correct = extract { date: String =>
    date must beCorrect
  }

  def addition = extract { (a: String, b: String, c: String) =>
    a.toInt + b.toInt must_== c.toInt
  }
}}

The values to be extracted are delimited by `{}` and those curly braces will not be displayed when the specification is reported.

#### Standard delimited parsers

When you parse values with `extract` you get only Strings which you have to transform into `Int` for example. $specs2 comes up with a few predefined parsers to help you with that: ${snippet{
s2"""
 {1} plus {1} is {2}            $addition
"""

  import org.specs2.specification.script.StandardDelimitedStepParsers._

  def addition = threeInts.map { case (a, b, c) =>
    a + b must_== c
  }
}}

The other parsers are:

 parser        | description
 ------------- | -----------
 `anInt`       | return a `Int`
 `twoInts`     | return a pair `(Int, Int)`
 `threeInts`   | return a triple `(Int, Int, Int)`
 `aDouble`     | return a `Double`
 `twoDoubles`  | return a pair `(Double, Double)`
 `threeDoubles`| return a triple `(Double, Double, Double)`
 `aString`     | return a `String`
 `twoStrings`  | return a pair `(String, String)`
 `threeStrings`| return a triple `(String, String, String)`


#### Regular expressions

Another way to extract values is to use regular expressions to extract similar groups of values. In that case no delimiters are required. For example: ${snippet{
s2"""
 1971-01-01 is a correct date $correct
 1 plus 1 is 2                $addition
"""

  import org.specs2.specification.script.StepParsers._

  // groupAs is equivalent to running 'regexp findAllIn text'
  // and getting one argument per match group found
  def correct = groupAs("[^ ]+").and { date: String =>
    date must beCorrect
  }

  def addition = groupAs("\\d+").and { (a: String, b: String, c: String) =>
    a.toInt + b.toInt must_== c.toInt
  }
}}

#### Standard regexp parsers

Similarly to delimited parsers, there are some predefined regexp parsers: ${snippet{
s2"""
 1 plus 1 is 2            $addition
"""

  import org.specs2.specification.script.StandardRegexStepParsers._

  def addition = threeInts.map { case (a, b, c) =>
    a + b must_== c
  }
}}
"""

  def beCorrect: Matcher[String] = ???

}
