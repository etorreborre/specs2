package org.specs2
package guide
package matchers

import matcher._

object JsonMatchers extends UserGuideCard with matcher.JsonMatchers {
  def title = "Json"
  def text =  s2"""
 [Json](http://www.json.org) is a simple data format essentially modeling recursive key-values. There are 2 matchers which can be used to verify the presence of appropriate values in Strings representing Json documents:

 * `/(value)` checks if a value is present at the root of the document. This can only be the case if that document is an Array

 * `/(regex)` checks if a value matching the regex is present at the root of the document. This can only be the case if that document is an Array

 * `/(key -> value)` checks if a pair is present at the root of the document. This can only be the case if that document is a Map

 * `*/(value)` checks if a value is present anywhere in the document, either as an entry in an Array, or as the value for a key in a Map

 * `*/(key -> value)` checks if a pair is present anywhere in a Map of the document

 * `/#(i)` selects the ith element in a 0-based indexed Array or a Map and allow further checks on that element

Now the interesting part comes from the fact that those matchers can be chained to search specific paths in the Json document. For example, for the following document: ${snippet{

// taken from an example in the Lift project
val person = """{
  "person": {
    "name": "Joe",
    "age": 35,
    "spouse": {
      "person": {
        "name": "Marilyn",
        "age": 33
      }
    }
  }
}
"""
// 8<--
()
}}

You can use these combinations: ${snippet{
person must /("person") */("person") /("age" -> 33.0) // by default numbers are parsed as Doubles
}}

You can as well use regular expressions or String matchers instead of values to verify the presence of keys or elements. For example: ${snippet{
person must /("p.*".r) */ ".*on".r /("age" -> "\\d+\\.\\d".r)
person must /("p.*".r) */ ".*on".r /("age" -> startWith("3"))
person must /("p.*".r) */ ".*on".r /("age" -> (be_>(30) ^^ ((_:String).toInt)))
}}

You can also access some records by their index: ${snippet{
person must /("person") /# 2 / "person"
}}

Finally you can use Json matchers to match elements in an array: ${snippet{
val json = """{"products":[{"name":"shirt","price":10, "ids":["1", "2", "3"]},{"name":"shoe","price":5}]}"""

def aProductWith(name: Matcher[JsonType],  price: Matcher[JsonType]): Matcher[String] =
  /("name").andHave(name) and /("price").andHave(price)

def haveProducts(products: Matcher[String]*): Matcher[String] =
  /("products").andHave(allOf(products:_*))

json must haveProducts(
  aProductWith(name = "shirt", price = 10) and /("ids").andHave(exactly("1", "2", "3")),
  aProductWith(name = "shoe", price = 5)
)
}}

The `andHave` method accepts any `Matcher[JsonType]` where `JsonType` is either `JsonArray`, `JsonMap`, `JsonNumber`, `JsonString`, `JsonNull`. In the example above we pass directly `shirt` and `10` as `Matcher[JsonType]` because there are implicit conversions from `Int`, `Boolean`, `Double`, `String` and `Traversable[String]` matchers (like `allOf`) to a `Matcher[JsonType]`.

"""
  lazy val person = ""
}
