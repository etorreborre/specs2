package org.specs2
package guide.old

class JsonExamples extends Specification with matcher.JsonMatchers {
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
    }"""

    def is =
    "1" ! { person must /("person") */("person") /("age" -> 33.0) }
    "2" ! { person must /("person") /#(2) /("person") }
}
