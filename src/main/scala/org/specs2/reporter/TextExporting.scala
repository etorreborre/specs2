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
trait TextExporting extends 
         FoldExporting 
    with TextPrinter
    with Output { outer =>

  /**
   * Folding function to accumulate statistics and print the results to the output at
   * the same time
   */
  val folder = new ExecutedFragmentFold {
    type T = outer.T
    def initial = outer.initial
    
    def fold(implicit args: Arguments): Function2[T, ExecutedFragment, T] = {
      case (s, executed) => {
        val newStats: T = outer.fold(args)(s, executed)
        print(args)(newStats, executed)
        newStats
      }
    }
  }
} 
