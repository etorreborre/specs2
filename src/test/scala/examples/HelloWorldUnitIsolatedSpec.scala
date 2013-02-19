package examples

import org.specs2._

class HelloWorldUnitIsolatedSpec extends mutable.Specification {
  isolated

  "A variable with the 'Hello world' string" >> {
    var l = "Hello world"
    "can be modified" in {
      l = "Hello you"
      l must have size (9)
    }
    "several times" in {
      l = "Hiya"
      l must have size (4)
    }
    "or not" in {
      l must have size (11)
    }
  }

}
