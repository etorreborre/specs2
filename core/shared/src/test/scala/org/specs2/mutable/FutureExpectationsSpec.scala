package org.specs2
package mutable

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

@annotation.nowarn
class FutureExpectationsImmutableSpec extends org.specs2.Specification { def is = s2"""

 A specification can return future results
   For example here $example1

"""

  def example1 =
    Future.apply(1 === 1)

}

@annotation.nowarn
class FutureExpectationsMutableSpec extends org.specs2.mutable.Specification {

  "A specification can return future results" >> {
    "For example here" in {
      Future.apply(1 === 1)(global)
    }
    "For example there" in { withTitle: String =>
      Future.apply(withTitle === "For example there")(global)
    }

  }

}
