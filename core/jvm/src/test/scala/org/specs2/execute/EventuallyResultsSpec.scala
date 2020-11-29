package org.specs2
package execute

import mutable.Spec
import org.specs2.matcher.ResultMatchers
import scala.concurrent._, duration._

class EventuallyResultsSpec extends Spec with ResultMatchers:
  addText("""
  `eventually` can be used to retry any result until a maximum number of times is reached
    or until it succeeds.
  """)

  "A success succeeds right away with eventually" >> {
    eventually(Success())
  }

  "A failure will always fail" >> {
    eventually(Failure()).not
  }

  "A result will be retried automatically until it succeeds" >> {
    val iterator = List(Failure(), Failure(), Success()).iterator
    eventually(iterator.next)
  }

  "If all retries fail, the result will eventually fail" >> {
    val iterator = Iterator.continually(Failure())
    eventually(iterator.next).not
  }

  "Any object convertible to a result can be used with eventually" >> {
    val iterator = List(false, false, true).iterator
    eventually(iterator.next) must beSuccessful
  }

  "Even if a result throws an exception it must be evaluated 'retries' times only" >> {
    var eval = 0
    def r = { eval += 1; 1 must ===(2) }

    eventually(retries = 3, sleep = 100.millis)(r) must beFailing
    eval must ===(3)
  }
