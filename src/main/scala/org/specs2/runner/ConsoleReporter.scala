package org.specs2
package runner
import specification._
import io._
import function._
import scalaz.Scalaz._

trait ConsoleReporter extends Reporter with ConsoleOutput with AnExecutor with APrinter with ALevelParser {
  
  /**
   *                          (Level[Int], ExecutedFragment) -> print
   *                        /       
   * (T, Fragment) -> level                                       x
   *                        \
   *                          updated[T]                     -> identity -> T
   * 
   */
  lazy val folder: Function2[T, Fragment, T] = {
	case p => ((execute >>> (printer.print *** identity)) apply p)._2
  }

  type T = Level
  def initial = levelParser.initial
  
  val execute: Function[(T, Fragment), ((Int, ExecutedFragment), T)] = {
	case p @ (a, f) => ((levelParser.currentLevel(p), executor.execute(f)), levelParser.updateLevel(p))
  }
}

trait AConsoleReporter extends AReporter {
  val reporter = new ConsoleReporter {}
}
