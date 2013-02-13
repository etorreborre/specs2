package org.specs2
package matcher

class JsonMatchersSpec extends Specification { def is =

 "The / matcher matches a name and a value if the input is a Map"                                                       ^
 { """{"name" : "Joe"}""" must /("name" -> "Joe") }                                                                     ^
 { """{"name" : "Joe"}""" must not /("name2" -> "Joe") }                                                                ^
 { ("""["name", "Joe"]""" must /("name" -> "Joe")) returns "[name, Joe] doesn't contain 'name' : 'Joe'" }               ^
 { ("garbage" must /("name" -> "joe")) returns "Could not parse:\ngarbage" }                                            ^
 "with regexes as well"                                                                                                 ^
 { """{"name" : "Joe"}""" must /("n.*".r -> "J.*".r) }                                                                  ^
 { """{"name" : "Joe"}""" must /("n.*".r -> "j.*".r) returns "{name : Joe} doesn't contain 'n.*'.r : 'j.*'.r" }         ^
                                                                                                                        endp^
 "The / matcher matches a value if the input is an Array"                                                               ^
 { """["name", "Joe" ]""" must /("name") }                                                                              ^
 { """[1.0, 2.0]""" must /(1.0) }                                                                                       ^
 { """{"name" : "Joe"}""" must not /("name") }                                                                          ^
 { ("""{"name" : "Joe"}""" must /("name")) returns "{name : Joe} doesn't contain 'name'" }                              ^
 { ("garbage" must /("name")) returns "Could not parse:\ngarbage" }                                                     ^
 "with regexes as well"                                                                                                 ^
 { """["name", "Joe"]""" must /("J.*".r) }                                                                              ^
 { """["name", "Joe"]""" must /("j.*".r) returns "[name, Joe] doesn't contain 'j.*'.r" }                                ^
                                                                                                                        endp^
 "The / matcher can be chained with another /"                                                                          ^
 { """{"person" : {"name": "Joe"}}""" must /("person") /("name" -> "Joe") }                                             ^
 { """{"person" : ["name"]}""" must /("person") /("name") }                                                             ^
 { """{"person" : { "name": "Joe", "address" : {"street": "here" }}}""" must
    /("person") /("address") /("street" -> "here") }                                                                    ^
 { """{"person" : {"name": "Joe", "address": ["street"]}}""" must
      /("person") /("address") /("street") }                                                                            ^
                                                                                                                        endp^
 "The */ matcher matches a name and a value inside any Map"                                                             ^
 { """{"name" : "Joe" }""" must */("name" -> "Joe") }                                                                   ^
 { """{"person" : {"name": "Joe"}}""" must */("name" -> "Joe") }                                                        ^
 { """{"person" : {"name": "Joe"}}""" must */("person" -> ".*".r) }                                                     ^
 { """{"person" : {"name": "Joe"}}""" must */("name" -> ".*".r) }                                                       ^
 { ("""{"person" : ["name", "Joe"]}""" must not */("name" -> "Joe")) }                                                  ^
 { ("""{"person" : ["name", "Joe"]}""" must */("name" -> "Joe")) returns
   "{person : [name, Joe]} doesn't contain 'name' : 'Joe'" }                                                            ^
 { ("garbage" must */("name" -> "joe")) returns "Could not parse:\ngarbage" }                                           ^
                                                                                                                        endp^
 "The */ matcher can be chained with /"                                                                                 ^
 { """{"person" : {"name": "Joe"}}""" must */("name") /("Joe") }                                                        ^
 { """{"person" : {"name": "Joe", "name": "Moe"}}""" must */("name") /("Moe") }                                         ^
                                                                                                                        endp^
 "The */ matcher can be chained with */"                                                                                ^
 { """{"person": {"address" : {"street" : "here" }}}""" must */("person") */("address") /("street", "here") }           ^
                                                                                                                        endp^
 "The / matcher can be chained with */"                                                                                 ^
 { """{"person" : {"address" : {"street" : "here"}}}""" must /("person") */("street") /("here") }                       ^
                                                                                                                        endp^
 "The /#(i) matcher matches the i-th element in an Array"                                                               ^
 { """["name", "Joe"]""" must /#(1) /("Joe") }                                                                          ^
 { """{"person" : ["name", "Joe"] }""" must /("person") /#(1) /("Joe") }                                                ^
 "xxx"!{ """{"person" : ["name", ["Joe", "Moe"]] }""" must /("person") /#(1) /#(1) /("Moe") }                                 ^
 { """{"house" : {"person" : ["name", "Joe"]}}""" must */("person") /#(1) /("Joe") }                                    ^
                                                                                                                        endp^
 "The /#(i) matcher matches the i-th element in a Map"                                                                  ^
 { """{"name" : "Joe", "name2" : "Moe"}""" must /#(1) /("name2" -> "Moe") }                                             ^
 { """{"person" : {"name": "Joe", "name2" : "Moe"} }""" must /("person") /#(1) /("name2", "Moe") }                      ^
 { """{"house" : {"person" : {"name": "Joe", "name2" : "Moe"}}}""" must */("person") /#(1) /("name2", "Moe") }          ^
                                                                                                                        end

  // this example is taken from the liftweb project
  val person = """
{
  "person" : {
    "name" : "Joe",
    "age" : 35,
    "spouse" : {
      "person" : {
        "name" : "Marilyn",
        "age" : 33
      }
    }
  }
}
"""


}