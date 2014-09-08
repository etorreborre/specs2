package examples

import org.specs2._
import specification.AllExpectations

class HelloWorldUnitAllExpectationsSpec extends mutable.Specification with AllExpectations {

  "In the following examples, all the expectations must be checked\n"+
  "change some of them to see all the failures being reported"       >> {

    "Hello world" >> {
      "Hello world".reverse === "dlrow olleH"
      "Hello world".size === 11
      "Hello world".split("\\s") must haveSize(2)
      "Hello world".toLowerCase === "hello world"
    }

  }

}
