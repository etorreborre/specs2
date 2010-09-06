package org.specs2.specification

import org.specs2._

object ExamplesExecutionSpec extends Specification {
  val examples: Examples = 
  "An example when executed returns a result" ~ e1
  
  def e1 = new Result("ok")
}