package examples

import org.specs2._

class HelloWorldUnitSpec extends mutable.Specification {

  "The 'Hello world' string" should {
    "contain 11 characters" in {
      "Hello world" must have size(11)
    }
    "start with 'Hello'" in {
      "Hello world" must startWith("Hello")
    }
    "end with 'world'" in {
      "Hello world" must endWith("world")
    }
  }
  
}
