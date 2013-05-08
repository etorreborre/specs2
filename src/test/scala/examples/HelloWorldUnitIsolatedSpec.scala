package examples

import org.specs2._

/**
 * This specification shows that it is possible to isolate the execution of each example so that local variables
 * (in this case the variable named `local`) can be safely used
 */
class HelloWorldUnitIsolatedSpec extends mutable.Specification {
  isolated

  "A variable with the 'Hello world' string" >> {
    var local = "Hello world"
    "can be modified" in {
      local = "Hello you"
      local must have size (9)
    }
    "several times" in {
      local = "Hiya"
      local must have size (4)
    }
    "or not" in {
      local must have size (11)
    }
  }

}
