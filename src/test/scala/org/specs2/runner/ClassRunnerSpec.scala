package org.specs2
package runner

import reporter._
import mock.Mockito

class ClassRunnerSpec extends Specification with Mockito { def is =

  "Exit codes" ^
    "When a run is finished the runner exits the system with an exit code" ^
      "0 for a success"  ! e1^
      "1 for a failure"  ! e2^
      "100 for an error" ! e3^
                          end

  def e1 = runClass("OkSpecification") === 0
  def e2 = runClass("KoSpecification") === 1
  def e3 = runClass("ErrorSpecification") === 100

  def runClass(className: String) = {
    val r = newRunner
    r.main(Array("user.reporter."+className))
    r.status
  }

  def newRunner = new ClassRunner {
    override lazy val reporter: Reporter = new ConsoleReporter {
      override def textOutput = new NoResultOutput
    }
    // if there's an error the statuses are going to be Seq(100, 0) because we don't really exit,...
    val statuses = new scala.collection.mutable.ListBuffer[Int]
    def status = statuses.head
    override def exitWith(s: Int) { statuses += s }
  }
}
