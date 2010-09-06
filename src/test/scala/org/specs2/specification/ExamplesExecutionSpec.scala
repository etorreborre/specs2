package org.specs2.specification

import org.specs2._

object ExamplesExecutionSpec extends Specification {
  val examples: Examples = 
  "An example when executed returns a result" ~ e1
  
  def e1 = (1 must_== 1) must_== Success("1 is equal to 1")
}