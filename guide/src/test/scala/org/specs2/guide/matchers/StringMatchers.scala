package org.specs2
package guide
package matchers

object StringMatchers extends UserGuideCard {
  def title = "String"
  def text = s2"""
Matching on strings is very common. Here are the matchers which can help you:

 Matcher                               | Description
 ------------------------------------- | ------------
 `beMatching` or ` be matching`        | check if a string matches a regular expression
 `=~(s)`                               | shortcut for `beMatching("(.|\\s)*"+s+"(.|\\s)*")`
 `find(exp).withGroups(a, b, c)`       | check if some groups are found in a string
 `have length`                         | check the length of a string
 `have size`                           | check the size of a string (seen as an `Iterable[Char]`)
 `be empty`                            | check if a string is empty
 `beEqualTo(b).ignoreCase`             | check if 2 strings are equal regardless of casing
 `beEqualTo(b).ignoreSpace`            | check if 2 strings are equal when you `replaceAll("\\s", "")`
 `beEqualTo(b).trimmed`                | check if 2 strings are equal when trimmed
 `beEqualTo(b).ignoreSpace.ignoreCase` | you can compose them
 `contain(b)`                          | check if a string contains another one
 `startWith(b)`                        | check if a string starts with another one
 `endWith(b)`                          | check if a string ends with another one
"""
}
