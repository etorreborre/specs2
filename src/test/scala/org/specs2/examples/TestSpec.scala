package org.specs2
package examples

import org.specs2._

class TestSpec extends mutable.Specification  {
val random = new scala.util.Random

  "Random tests" >> {
    (1 to 10) foreach { i =>
      "test "+i >> {
        Thread.sleep(50*random.nextInt(i))
        success
      }
    }

  }
}
