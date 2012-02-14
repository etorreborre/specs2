package org.specs2.examples

import org.specs2._

class HelloWorldUnitIsolatedSpec extends mutable.Specification {

  "The 'Hello world' string" should {
    var l = new scala.collection.mutable.ListBuffer[Int]
    "contain 11 characters" in {
      l = l :+ 1
      l must have size (1)
    }
    "start with 'Hello'" in {
      l = l :+ 1
      l must have size (1)
    }
    "end with 'world'" in {
      l = l :+ 1
      l must have size (1)
    }
  }

}
