package org.specs2
package specification
import execute._
import matcher._
import mutable._

class FragmentsExecutionSpec extends Specification {

  "An example when executed returns a result" in {
    (1 must_== 1).toResult must_== Success("'1' is equal to '1'")
  }

  "A failed expectation must point to its precise location" in {
    val result: Failure = new user.specification.UserExecutionSpecification().is.fragments.
      collect { case Example(_, body) if (body().isFailure) => body().asInstanceOf[Failure] }.apply(0)
    result.location must beMatching("UserExecutionSpecification.scala:6.*")
  }

  "An example can be marked as pending until fixed" in e1
    "with a specific message" in e2
    "it must change to failed when the example succeeds" in e3
    "with a specific message" in e4

  "An executed result must have its own Stats" in {
    FragmentExecution.execute("e1" ! failure).stats.failures must_== 1
  }


  def e1 = {
    val ex = "ex" ! { 1 must_== 2 }.pendingUntilFixed
    ex.execute must_== Pending("Pending until fixed")
  }
  def e2 = {
    val ex = "ex" ! { 1 must_== 2 }.pendingUntilFixed("ISSUE-123")
    ex.execute must_== Pending("ISSUE-123. Pending until fixed")
  }
  def e3 = {
    val ex = "ex" ! { 1 must_== 1 }.pendingUntilFixed
    ex.execute must_== Failure("Fixed now, you should remove the 'pendingUntilFixed' marker")
  }
  def e4 = {
    val ex = "ex" ! { 1 must_== 1 }.pendingUntilFixed("ISSUE-123")
    ex.execute must_== Failure("ISSUE-123. Fixed now, you should remove the 'pendingUntilFixed' marker")
  }
}
