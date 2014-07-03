package org.specs2
package execute

import mutable.Specification
import org.specs2.specification.core.Fragment
import org.specs2.specification.process.DefaultExecutor

class PendingUntilFixedSpec extends Specification {

  "An example can be marked as pending until fixed" in e1
  "with a specific message" in e2
  "it must change to failed when the example succeeds" in e3
  "with a specific message" in e4
  "An AssertionError must be interpreted as non-fixed" in e5

  def e1 = {
    val ex = "ex" ! { 1 must_== 2 }.pendingUntilFixed
    execute(ex) must_== Pending("Pending until fixed")
  }
  def e2 = {
    val ex = "ex" ! { 1 must_== 2 }.pendingUntilFixed("ISSUE-123")
    execute(ex) must_== Pending("ISSUE-123. Pending until fixed")
  }
  def e3 = {
    val ex = "ex" ! { 1 must_== 1 }.pendingUntilFixed
    execute(ex) must_== Failure("Fixed now, you should remove the 'pendingUntilFixed' marker")
  }
  def e4 = {
    val ex = "ex" ! { 1 must_== 1 }.pendingUntilFixed("ISSUE-123")
    execute(ex) must_== Failure("ISSUE-123. Fixed now, you should remove the 'pendingUntilFixed' marker")
  }
  def e5 = {
    val ex = "ex" ! { assert(false); 1 must_== 2 }.pendingUntilFixed
    execute(ex) must_== Pending("Pending until fixed")
  }

  def execute(f: Fragment) =
    DefaultExecutor.executeAll(f).head.executionResult

}
