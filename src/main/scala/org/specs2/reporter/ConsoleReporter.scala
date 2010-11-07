package org.specs2
package reporter

import scalaz.Scalaz._
import io._
import main._
import specification._

/**
 * The console reporter executes a Specification and exports the results to the Console
 * Output:
 * 
 * * DefaultSelection filters and sorts the Fragments
 * * DefaultExecutionStrategy executes the Examples concurrently by default
 * * TextExporting prints the results in a Tree manner
 * * ConsoleOutput specifies that the output must be the standard output console
 *
 */
private[specs2]
trait ConsoleReporter extends Reporter 
    with DefaultSelection
    with DefaultExecutionStrategy
    with TextExporting
    with ConsoleOutput 
  

/**
 * A trait holding a ConsoleReporter
 */
private[specs2]
trait AConsoleReporter {
  lazy val reporter = new ConsoleReporter {}
}
