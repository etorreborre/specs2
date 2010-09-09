package org.specs2
package runner
import specification._
import io._
import function._
import scalaz.Scalaz._

trait ConsoleReporter extends Reporter with ConsoleOutput with ExampleExecution with Functions with NestedPrinter {
  import NestedLevels._
  
  /**
   *                          (Level[Int], ExecutedFragment) -> print
   *                        /       
   * (T, Fragment) -> level                                       x
   *                        \
   *                          updated[T]                     -> identity -> T
   * 
   */
  lazy val folder: PartialFunction[(T, Fragment), T] = {
	case p => ((level >>> (print *** identity)) apply p)._2
  }

  type T = Accumulator
  def initial = new Accumulator()
  
  val level: PartialFunction[(T, Fragment), ((Int, ExecutedFragment), T)] = {
	case p @ (a, f) => ((currentLevel(p), execute(f)), updateLevel(p))
  }
}

trait AConsoleReporter extends AReporter {
  val reporter = new ConsoleReporter {}
}
