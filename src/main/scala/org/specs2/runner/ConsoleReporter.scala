package org.specs2
package runner
import specification._
import io._
import function._
import scalaz.Scalaz._

trait ConsoleReporter extends Reporter with ConsoleOutput with AnExecutor with NestedPrinter with NestedLevels 
  with TotalStatistics {
  
  /**
   *                          (Level[Int], ExecutedFragment) -> print
   *                        /       
   * (T, Fragment) -> level                                       x
   *                        \
   *                          updated[T]                     -> identity -> T
   * 
   */
  lazy val folder: Function2[T, Fragment, T] = {
	case p => ((execute >>> print *** identity) apply p)._2
  }

  type T = (Level, Stats)
  def initial = (Level(), Stats())
  
  val execute: Function[(T, Fragment), ((Int, Stats, ExecutedFragment), T)] = {
	case p @ ((l, s), f) => {
	  val newLevel = level((l, f))
	  val executed = executor.execute(f)
	  val newStats = stats((s, executed))
	  ((newLevel._1, newStats, executed), (newLevel._2, newStats))
	}
  }
}

trait AConsoleReporter extends AReporter {
  val reporter = new ConsoleReporter {}
}
