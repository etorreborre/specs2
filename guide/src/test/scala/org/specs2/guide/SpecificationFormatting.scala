package org.specs2
package guide

object SpecificationFormatting extends UserGuidePage { def is = s2"""
It is fairly easy to make the text of an acceptance specification look like you want in the console thanks to interpolated strings. Doing to same thing is not obvious at first in a unit specification. How do you add a new line after an example? After the specification title? How do you indent a group of examples a bit more?

First of all, you can always add a piece text by using the `txt` method on a `String`:${snippet{
class UnitSpec extends mutable.Specification {
  """
 This is a long and important introduction to this specification.
 The examples below show everything you can do with the system.
  """.txt
}
}}

Then if you want to add new lines you can use:

 - `br` ("break") to add a newline after any fragment, text, example...
 - `p` ("paragraph") to make a new paragraph with a break before and 2 after

Texts, or blocks of examples can also get a special indentation by using the `tab` and `backtab` methods:${snippet{
class UnitSpec extends mutable.Specification {
"""
This is a long and important introduction to this specification.
""".text

"""
The examples below show everything you can do with the system.
""".txt.tab(3) // indent the text with 3 tabs compared to the previous text
}
}}

"""
}

