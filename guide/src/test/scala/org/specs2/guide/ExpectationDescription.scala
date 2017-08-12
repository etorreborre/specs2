package org.specs2
package guide

import org.specs2.control.Functions._

object ExpectationDescription extends UserGuidePage { def is = s2"""
#### Enhance failures messages

Some expressions using matchers might not produce very useful messages. For example: ${snippet{
val ticketsNumber = 5

// will fail with '5' is not equal to '3'
ticketsNumber must be_==(3)
}}

You can improve this failure message by describing what `ticketsNumber` represents: ${snippet{
val ticketsNumber = 5

// will fail with "the number of tickets '5' is not equal to '3'"
ticketsNumber aka "the number of tickets" must be_==(3)
}}

The `aka` (*also known as*) method has a few variations:

* `value.aka` is a shortcut for `value aka value.toString`
* `"a" post "is the first letter"` prints `a is the first letter`
* `"b" as ((s:String) => "a"+s+"c")` prints `abc`
* `Seq(1, 2, 3, 4).showAs((_:Seq[Int]).filter(isEven).mkString("|"))` prints `2|4`. This one is especially useful to filter out big data structures (lists, maps, xml...) before the displaying the failure message

#### Update the failure message

On a `Matcher`, a `MatchResult` or a `Result` you can use `updateMessage(f: String => String)` or `setMessage(m: String)` to change the failure message.

#### Describe an expectation

Another way to provide a description for an expectation is to use the `==>` (or `<==>`) operator:${snippet{
s2"""A byname function can be transformed into a strict one $e1"""

def e1 = {
  def byNameFunction(u: =>Unit) {}
  var parameter = "not evaluated"
  toStrictFunction1(byNameFunction){ parameter = "evaluated" }

  "The byname function has become a strict one" ==> (parameter === "evaluated")
}
}}

In the example above, testing if `parameter == "evaluated"` is just a way to observe what we wanted to achieve. If that doesn't work, the failure message will be
```
[error] x The byname function has not become a strict one because 'evaluated' is not equal to 'not evaluated'
```
"""
}
