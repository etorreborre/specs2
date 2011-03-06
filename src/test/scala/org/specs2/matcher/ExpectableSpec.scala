package org.specs2
package matcher
import mutable._

class ExpectableSpec extends SpecificationWithJUnit {

  "An expectable can have a precise description with aka(description)" in {
    ("a" aka "the string").description must_== "the string 'a'"
  }
  "If it is a boolean its value is not displayed, only the description" in {
    (true aka "my boolean").description must_== "my boolean"
  }
  "An expectable described with aka only will take it's own toString value as the description" in {
    ("a" aka).description must_== "a 'a'"
  }
  "An expectable described with post will have some description text appended to its toString value" in {
    ("a" post " is the first letter").description must_== "a is the first letter"
  }
  "An expectable can be described with a function taking its toString value" in {
    ("b" as ((s:String) => "a"+s+"c")).description must_== "abc"
  }

}