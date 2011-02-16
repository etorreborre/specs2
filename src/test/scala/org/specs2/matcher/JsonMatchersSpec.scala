package org.specs2
package matcher

class JsonMatchersSpec extends SpecificationWithJUnit { def is =

 "The / matcher matches a name and a value if the input is a Map"                                                       ^
 { """{ "name": "Joe" }""" must /("name" -> "Joe") }                                                                    ^
 { """{ "name": "Joe" }""" must not /("name2" -> "Joe") }                                                               ^
 { ("""["name", "Joe" ]""" must /("name" -> "Joe")) returns "[name, Joe] doesn't contain: name -> Joe" }                ^
 { ("garbage" must /("name" -> "joe")) returns "Could not parse:\ngarbage" }                                            ^
                                                                                                                        p^
 "The / matcher matches a value if the input is an Array"                                                               ^
 { """["name", "Joe" ]""" must /("name") }                                                                              ^
 { """{ "name": "Joe" }""" must not /("name") }                                                                         ^
 { ("""{ "name": "Joe" }""" must /("name")) returns "{ name: Joe } doesn't contain: 'name'" }                           ^
 { ("garbage" must /("name")) returns "Could not parse:\ngarbage" }                                                     ^
                                                                                                                        p^
 "The / matcher can be chained"                                                                                         ^
 { """{ "person": { "name": "Joe" } }""" must /("person") /("name" -> "Joe") }                                          ^
 { """{ "person": ["name"] }""" must /("person") /("name") }                                                            ^
 { """{ "person": { "name": "Joe", "address": { "street": "here" } } }""" must
    /("person") /("address") /("street" -> "here") }                                                                    ^
 { """{ "person": { "name": "Joe", "address": ["street"] } }""" must
      /("person") /("address") /("street") }                                                                            ^
                                                                                                                        end

  // this example is taken from the liftweb project
  val person = """
{
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


}