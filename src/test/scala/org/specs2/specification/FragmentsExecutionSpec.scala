package org.specs2
package specification

import execute._
import _root_.org.specs2.mutable.{Specification => Spec}

class FragmentsExecutionSpec extends Spec {

  "An example when executed returns a result" in {
    (1 must_== 1).toResult must_== Success("'1' is equal to '1'")
  }

  "A failed expectation must point to its precise location" in {
    val result: Failure = new user.specification.UserExecutionSpecification().is.fragments.
      collect { case Example(_, body) if (body().isFailure) => body().asInstanceOf[Failure] }.apply(0)
    result.location must beMatching("UserExecutionSpecification.scala:6.*")
  }

  "An executed result must have its own Stats" in {
    FragmentExecution.execute("e1" ! failure).stats.failures must_== 1
  }

  "An executed Step must return a Result" >> {
    "which is a Result if the step evaluates a value of type Result" >> {
      Step(success).execute === success
    }
    "which is a DecoratedResult[T] if the step evaluates a value of type T" >> {
      Step(1).execute === DecoratedResult(1, Success())
    }
   }

}
