package org.specs2
package reporter

import io._
import main.Arguments
import specification.{ExecutedFragment, Fragment}
import specification.ExecutedFragment._

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
trait ConsoleReporter extends DefaultReporter 
    with TextExporting
    with ConsoleOutput {
}
