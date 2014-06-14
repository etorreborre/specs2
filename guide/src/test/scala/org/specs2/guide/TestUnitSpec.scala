package org.specs2
package guide

import org.specs2.specification.BeforeEach

class TestUnitSpec extends mutable.Specification with BeforeEach {
  // you need to define the "before" action
  def before = println("before")

  "example 1" >> { println("example1"); ok }
  "example 2" >> { println("example2"); ok }
}
