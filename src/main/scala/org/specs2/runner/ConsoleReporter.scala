package org.specs2
package runner
import specification._
import io._
import function._

trait ConsoleReporter extends Reporter with ConsoleOutput with ExampleExecution with Functions {
  
  case class Accumulator(level: Int = 0, state: Direction = Up, lastNode: LastNode = T)
  type T = Accumulator
  val initial = new Accumulator()
  
  lazy val folder: PartialFunction[(T, Fragment), T] = (level into print) then update
  
  val print: PartialFunction[(Int, Fragment), Unit] = { 
	case (level, Text(s)) => println(("  " * level) + s)
	case (level, e @ Example(s, Some(body))) => {
	  val result = execute(body)
	  println("  " * level + status(result) + " " + s)
	  printMessage(level + 1, result)
	}
	case (a, `br`) => println("")
  }
  private def printMessage(level: Int, result: Result) = {
	result match {
	  case Success(_) => ()
	  case _ => println("  " * level + result.message)
	}
  }
  private def status(result: Result) = {
	result match {
	  case Success(_) => "+"
	  case Failure(_) => "x"
	  case Error(_)   => "x"
	  case Pending(_) => "o"
	  case Skipped(_) => "o"
	}
  }
  
  val level: PartialFunction[(T, Fragment), (Int, Fragment)] = { 
	case (a, f @ Text(s)) if (a.state == Down && a.lastNode == E) => (a.level - 1, f)
	case (a, f) => (a.level, f)
  }
  
  val update: PartialFunction[(Accumulator, Fragment), Accumulator] = {
	case (a, `par`) => new Accumulator()
	case (a, `br`) => a
	case (a, Text(s)) => {
	  a.state match {
	 	case Up => a.copy(level = a.level + 1, lastNode = T)
	 	case Down => {
  	 	   if (a.lastNode == E)
	 	     a.copy(lastNode = T)
  	 	   else 
	 	     a.copy(level = a.level + 1, lastNode = T, state = Up)
	 	}
	  }
	}
	case (a, e @ Example(s, Some(body))) => {
	  a.copy(state = Down, lastNode = E)
	}
  }
  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  sealed trait LastNode
  case object E extends LastNode
  case object T extends LastNode
}

trait AConsoleReporter extends AReporter {
  val reporter = new ConsoleReporter {}
}
