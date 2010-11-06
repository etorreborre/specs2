package org.specs2
package reporter

import scalaz.Scalaz._
import io._
import main._
import specification._

private[specs2]
trait ConsoleReporter extends Reporter with FoldExporting with DefaultExecutionStrategy with ConsoleOutput with TextPrinter
  with TotalStatistics {
  
  /**
   *                     (Stats, ExecutedFragment) -> print
   *                   /       
   * (T, Fragment) ->                                   x
   *                   \
   *                     updated[T]                -> identity -> T
   * 
   */
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

private[specs2]
trait AConsoleReporter extends AReporter {
  lazy val reporter: Reporter = new ConsoleReporter {}
}
