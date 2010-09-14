package org.specs2
package specification
import execute._
import matcher._

class ExamplesExecutionSpec extends Specification {
  val examples: Examples = 
  "An example when executed returns a result" ! {
    (1 must_== 1).toResult must_== Success("'1' is equal to '1'")
  }
  
}