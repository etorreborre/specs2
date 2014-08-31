package org.specs2
package data

import scalaz.concurrent.Task
import scalaz.stream.Process

class FoldSpec extends Spec { def is = s2"""

 A fold can be executed on a large Process $large

 """

  def large = {
    var n = 0
    val max = 500000
    val fold = Fold.fromFunction[Int](i => Task.delay(n = n+1))
    Fold.runFold(Process.range(0, max), fold).run
    n must_== max
  }
}
