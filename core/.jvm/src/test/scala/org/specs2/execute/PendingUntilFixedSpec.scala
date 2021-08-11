package org.specs2
package execute

import mutable.Specification
import org.specs2.specification.core.{Env, Execution, Fragment, OwnEnv}
import org.specs2.specification.process.DefaultExecutor
import scala.concurrent.duration.*

class PendingUntilFixedSpec(val env: Env) extends Specification with OwnEnv:

  "An example can be marked as pending until fixed" in e1
  "with a specific message" in e2
  "it must change to failed when the example succeeds" in e3
  "with a specific message" in e4
  "An AssertionError must be interpreted as non-fixed" in e5
  "pendingUntilFixed can be called as a prefix method" in e6
  "pendingUntilFixed can be called as a prefix method with a message" in e7
  "pendingUntilFixed can be used with an Execution" in e8
  "pendingUntilFixed can be used when an example times out" in e9

  def e1 =
    val ex = "ex" ! { 1 must ===(2) }.pendingUntilFixed
    execute(ex) must ===(Pending("Pending until fixed"))

  def e2 =
    val ex = "ex" ! { 1 must ===(2) }.pendingUntilFixed("ISSUE-123")
    execute(ex) must ===(Pending("ISSUE-123. Pending until fixed"))

  def e3 =
    val ex = "ex" ! { 1 must ===(1) }.pendingUntilFixed
    execute(ex) must ===(Failure("Fixed now, you should remove the 'pendingUntilFixed' marker"))

  def e4 =
    val ex = "ex" ! { 1 must ===(1) }.pendingUntilFixed("ISSUE-123")
    execute(ex) must ===(Failure("ISSUE-123. Fixed now, you should remove the 'pendingUntilFixed' marker"))

  def e5 =
    val ex = "ex" ! { assert(false); 1 must ===(2) }.pendingUntilFixed
    execute(ex) must ===(Pending("Pending until fixed"))

  def e6 =
    val ex = "ex" ! pendingUntilFixed { 1 must ===(2) }
    execute(ex) must ===(Pending("Pending until fixed"))

  def e7 =
    val ex = "ex" ! pendingUntilFixed("ISSUE-123") { 1 must ===(2) }
    execute(ex) must ===(Pending("ISSUE-123. Pending until fixed"))

  def e8 =
    val ex = "ex" ! pendingUntilFixed("ISSUE-123") { Execution.result(1 must ===(2)) }
    execute(ex) must ===(Pending("ISSUE-123. Pending until fixed"))

  def e9 = {
    def tooLong: Boolean = { List.range(0, 10000000).reverse; true }
    pendingUntilFixed("this example times out") { Execution.result(tooLong).setTimeout(150.milliseconds) }
  }

  def execute(f: Fragment) =
    DefaultExecutor.executeAll(f)(ownEnv).head.executionResult.run(ownEnv.executionEnv)
