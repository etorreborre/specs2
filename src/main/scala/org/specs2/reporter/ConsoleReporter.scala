package org.specs2
package reporter

import main.Arguments
import scalaz._
import Scalaz._
import specification.{ExecutedSpecification, ExecutingSpecification, SpecificationStructure}
import scala.collection.parallel.ForkJoinTaskSupport
import control.Specs2ForkJoin

/**
* The console reporter executes a Specification and exports the results to the Console
* Output:
*
* - DefaultSelection filters and sorts the Fragments
* - DefaultExecutionStrategy executes the Examples concurrently by default
* - TextExporting prints the results in a Tree manner (using a TextOutput)
*
*/
trait ConsoleReporter extends DefaultReporter with TextExporting {

  override def report(spec: SpecificationStructure)(implicit arguments: Arguments): ExecutedSpecification = {
    // store the statistics and export the specification results in parallel to avoid
    // evaluating the whole execution sequence in the Storing trait before doing the printing
    // this allows to print the results as soon as executed
    val storeAndExport = (spec: ExecutingSpecification) => {
      val todo = Seq(store, export).par
      todo.tasksupport = new ForkJoinTaskSupport(Specs2ForkJoin.pool)
      todo.map(_(spec))
    }
    val toExecute = spec |> format |> select |> sequence |> execute
    toExecute |> storeAndExport
    toExecute.executed
  }

}

private [specs2]
trait SilentConsoleReporter extends DefaultReporter with NoExporting

private [specs2]
object SilentConsoleReporter extends SilentConsoleReporter
