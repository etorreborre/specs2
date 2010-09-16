package org.specs2
package runner
import specification._
import io._
import scalaz.Scalaz._
import main._

trait ConsoleReporter extends Reporter with ConsoleOutput with AnExecutor with TextPrinter
  with TotalStatistics {
  
  /**
   *                     (Stats, ExecutedFragment) -> print
   *                   /       
   * (T, Fragment) ->                                   x
   *                   \
   *                     updated[T]                -> identity -> T
   * 
   */
  lazy val folder: Function2[T, Fragment, T] = {
	case p => ((execute >>> print *** identity) apply p)._2
  }

  type T = Stats
  def initial = Stats()
  
  val execute: Function[(T, Fragment), ((Stats, ExecutedFragment), T)] = {
	case p @ (s, f) => {
	  val executed = executor.execute(f)
	  val newStats = stats((s, executed))
	  ((newStats, executed), newStats)
	}
  }
}

trait AConsoleReporter extends AReporter with Arguments {
  lazy val reporter: Reporter = new ConsoleReporter {
	override val configuration =  Configuration(arguments)
  }
}
