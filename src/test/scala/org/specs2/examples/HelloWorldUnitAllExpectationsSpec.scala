package examples

import org.specs2._
import specification.AllExpectations

class HelloWorldUnitAllExpectationsSpec extends mutable.Specification with AllExpectations {

  "In the following examples, all the expectations must be checked" >> {
    "An ok example" >> {
      ok
    }
    "Hello world" >> {
      "Hello world".reverse === "dlrow olleH"
      "Hello world".size === 12
      "Hello world".split("\\s") must have size(3)
      "Hello world".toLowerCase === "hello world"
    }
  }

}
