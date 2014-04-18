package org.specs2
package runner

import org.junit.runner.RunWith
import org.junit.experimental.categories.Category


@RunWith(classOf[JUnitRunner])
@Category(Array(classOf[FastTests]))
class JUnitRunnerTest extends mutable.Specification {
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

trait FastTests
