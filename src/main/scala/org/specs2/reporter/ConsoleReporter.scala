package org.specs2
package reporter

import scalaz.Scalaz._
import io._
import specification._
import main._

private[specs2]
trait ConsoleReporter extends Reporter with FolderExporting with DefaultExecutionStrategy with ConsoleOutput with TextPrinter
  with TotalStatistics {
  
  /**
   *                     (Stats, ExecutedFragment) -> print
   *                   /       
   * (T, Fragment) ->                                   x
   *                   \
   *                     updated[T]                -> identity -> T
   * 
   */
  val folder = new Folder[ExecutedFragment] {
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
