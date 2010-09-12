package org.specs2
package specification
import execute._

class ExamplesExecutionSpec extends Specification {
  val examples: Examples = 
  "An example when executed returns a result" ! e1
  
  def e1 = (1 must_== 1) must_== Success("'1' is equal to '1'")
}