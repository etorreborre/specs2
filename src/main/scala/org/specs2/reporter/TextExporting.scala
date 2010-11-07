package org.specs2
package reporter

import io.Output
import main.Arguments
import specification._

/**
 * This trait prints the executed fragments results and statistics
 * at the end of the specification
 */
private[specs2]
trait TextExporting extends FoldExporting 
    with TextPrinter
    with TotalStatistics 
    with Output {

  val fold = new ExecutedFragmentFold {
    type T = Stats
    def initial = Stats()
    def fold(implicit args: Arguments): Function2[T, ExecutedFragment, T] = {
      case p @ (s, executed) => {
        val newStats = stats(args)((s, executed))
        print(args)((newStats, executed))
        newStats
      }
    }
  }
} 
