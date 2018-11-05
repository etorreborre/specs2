package org.specs2
package mutable

import matcher._

import scala.concurrent.{ExecutionContext, Future}

class FutureExpectationsSpec(implicit ec: ExecutionContext) extends Specification with ResultMatchers {

  "A specification can return future results" >> {
    "For example here" in {
      Future.apply(1 === 1)
    }
    "For example there" in { withTitle: String =>
      Future.apply(ok)
    }

  }

}
