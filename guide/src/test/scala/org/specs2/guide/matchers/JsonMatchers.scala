package org.specs2
package guide
package matchers

object JsonMatchers extends UserGuideCard with matcher.JsonMatchers {
  def title = "Json"
  def text =  s2"""
 [Json](http://www.json.org) is a simple data format essentially modeling recursive key-values. Once you extend the `org.specs2.matcher.JsonMatchers` trait, you can verify the presence of appropriate values in Strings representing Json documents:

 Matcher                   | Description
 ------------------------- | -----------
 `/(value)`                | check if a value is present at the root of the document. This can only be the case if that document is an Array
 `/(regex)`                | check if a value matching the regex is present at the root of the document. This can only be the case if that document is an Array
 `/(key -> value)`         | check if a pair is present at the root of the document. This can only be the case if that document is a Map
 `*/(value)`               | check if a value is present anywhere in the document, either as an entry in an Array, or as the value for a key in a Map
 `*/(key -> value)`        | check if a pair is present anywhere in a Map of the document
 `/#(i)`                   | select the ith element in a 0-based indexed Array or a Map and allow further check on that element
 `have(matcher[JsonType])` | check the Json object (a `JsonArray` or a `JsonMap`) with a matcher. Check its type for example: `have(size(10)`

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

person must /("person") */ "person" /("age" -> 33.0) // by default numbers are parsed as Doubles



}}

You can as well use regular expressions or String matchers instead of values to verify the presence of keys or elements. For example:${snippet{

person must /("p.*".r) */ ".*on".r /("age" -> "\\d+\\.\\d".r)
person must /("p.*".r) */ ".*on".r /("age" -> startWith("3"))
person must /("p.*".r) */ ".*on".r /("age" -> (be_>(30) ^^ ((_:String).toInt)))

}}

You can check the size of a Json object:${snippet{

person must have(size(3))
// note that you need to explicitly need to chain operators
// as method calls here
person must /("person")./("spouse")./("person").andHave(size(2))

}}

Finally you can access some records by their index: ${snippet{

person must /("person") /# 2 / "person"

}}
"""
  lazy val person = ""
}
