package org.specs2
package reporter

import org.scalatools.testing.{Logger, EventHandler}
import main.Arguments
import specification.{SpecName, ExecutedFragment, Fragment, ExecutedSpecification}
import ExecutedFragment._

/**
 * Variation of the sbt TestInterfaceReporter where results are notified as soon as executed
 *
 */
class StreamingTestInterfaceReporter(override val handler: EventHandler, override val loggers: Array[Logger]) extends TestInterfaceReporter(handler, loggers) {

  /**
   * use a result output printing only the end statistics
   */
  override def print(name: SpecName, fs: Seq[ExecutedFragment])(implicit commandLineArgs: Arguments) =
    printLines(fs).print(new TestInterfaceStatsOnlyResultOutput(loggers))

  /**
   * execute a Fragment and print it right away (during the execution phase of the reporter)
   */
  override def executeFragment(implicit arguments: Arguments): Function[Fragment, ExecutedFragment] = (f: Fragment) => {
    val executed = super.executeFragment(arguments)(f)
    handleFragment(arguments)(executed)
    printLines(Seq(executed) filter { e => isExecutedText(e) || isExecutedResult(e) }).print(new TestInterfaceResultOutput(loggers))
    executed
  }

}

/**
 * This "TestInterface" result output only prints the last statistics during the export phase
 */
class TestInterfaceStatsOnlyResultOutput(override val loggers: Array[Logger]) extends TestInterfaceResultOutput(loggers) {
  override def printFailure(message: String)(implicit args: Arguments)   = ()
  override def printError(message: String)(implicit args: Arguments)     = ()
  override def printSuccess(message: String)(implicit args: Arguments)   = ()
  override def printText(message: String)(implicit args: Arguments)      = ()
}

