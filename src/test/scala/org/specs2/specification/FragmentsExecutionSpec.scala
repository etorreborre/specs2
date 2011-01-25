package org.specs2
package specification
import execute._
import matcher._

class FragmentsExecutionSpec extends SpecificationWithJUnit { def is =

  "An example when executed returns a result" ! {
    (1 must_== 1).toResult must_== Success("'1' is equal to '1'")
  }^
  "An example can be marked as pending until fixed" ! e1^
    "with a specific message" ! e2^
  p^
  "It must be failed when the example succeeds" ! e3^
    "with a specific message" ! e4^
  end

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
