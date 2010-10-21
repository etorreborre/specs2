package org.specs2
package reporter
import specification._
import io._
import scalaz.Scalaz._
import main._

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
    val fold: Function2[T, ExecutedFragment, T] = {
      case p @ (s, executed) => {
        val newStats = stats((s, executed))
        print((newStats, executed))
        newStats
      }
    }
  }
}

trait AConsoleReporter extends AReporter with Arguments {
  lazy val reporter: Reporter = new ConsoleReporter {
	  override val configuration =  Configuration(arguments)
  }
}
