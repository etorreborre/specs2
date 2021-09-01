package org.specs2
package execute

import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAll

class PendingUntilFixedBeforeAllSpec extends Specification with BeforeAll {
  private[this] var setupFinished = false

  override def beforeAll(): Unit = {
    setupFinished = true // will not execute as of 4.12.4
  }

  "Pending until fixed can be used with BeforeAll" >> {
    setupFinished ==== true
    (1 ==== 2).pendingUntilFixed("Will explode")
  }
}
