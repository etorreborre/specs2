package org.specs2
package specification

import matcher._

class FragmentsBuilderSpec extends script.Specification with ResultMatchers with Groups with GivenWhenThen {  def is = s2"""


With the GivenWhenThen trait

Start/End
=========
 + The title of the specification can not be empty

Examples
========
 + An example can use a partial function to extract values from its text
 + the description must be stripped out of value markers

                                                                                                           """

  "start/end" - new group {
    lazy val content = new Specification with GivenWhenThen {
      val number: Given[Int] = (_:String).toInt
      def is = "a number ${0}" ^ number
    }.content

    eg  :=  content.specStart.title must not(beEmpty)
  }
  "examples" - new group {
    val soExample = "given the name: ${eric}, then the age is ${18}" ! so {
      case (name: String, age: String) => age.toInt must_== 18
    }
    eg := soExample.body() must beSuccessful
    eg := soExample.desc.toString must_== "given the name: eric, then the age is 18"
  }

}