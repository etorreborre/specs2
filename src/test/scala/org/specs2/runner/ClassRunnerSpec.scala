package org.specs2
package runner

import reporter._

class ClassRunnerSpec extends Specification { def is = s2"""

  Exit codes
    When a run is finished the runner exits the system with an exit code
      0 for a success                                                          $e1
      1 for a failure                                                          $e2
      100 for an error                                                         $e3
                                                                               """

  def e1 = runClass("OkSpecification") === 0
  def e2 = runClass("KoSpecification") === 1
  def e3 = runClass("ErrorSpecification") === 100

  def runClass(className: String) = {
    val r = newRunner(className)
    r.main(Array("user.reporter."+className))
    r.status
  }

  def newRunner(className: String) = ExitRunner(className)
}

case class ExitRunner(name: String) extends ClassRunner {
  override lazy val reporter: Reporter = new ConsoleReporter {
    override def textOutput = new NoResultOutput
  }
  var status = 0
  var statusIsSet = false
  override def exitWith(s: Int) { if (!statusIsSet) { status = s; statusIsSet = true } }
}
