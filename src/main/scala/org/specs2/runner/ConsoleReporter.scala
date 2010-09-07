package org.specs2.runner
import org.specs2.Specification
import org.specs2.specification._

trait ConsoleReporter extends Reporter with ConsoleOutput with ExampleExecution {
	
  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  sealed trait LastNode
  case object E extends LastNode
  case object T extends LastNode
  
  case class Accumulator(level: Int = 0, state: Direction = Up, lastNode: LastNode = T)
  
  type T = Accumulator
  val initial = new Accumulator()
  
  val folder: FoldingFunction = {
	case (a, `par`) => {
	  new Accumulator()
	}
	case c @ (a, Text(s)) => {
	  a.state match {
	 	case Up => {
  	 	  println("  " * a.level + s)
	 	  a.copy(level = a.level + 1, lastNode = T)
	 	}
	 	case Down => {
  	 	   if (a.lastNode == E) {
  	 	  	 println("  " * (a.level-1) + s)
	 	     a.copy(level = a.level, lastNode = T)
  	 	   }
  	 	   else { 
  	 	  	 println("  " * (a.level) + s)
	 	     a.copy(level = a.level + 1, lastNode = T, state = Up)
	 	   }
	 	}
	  }
	}
	case c @ (a, e @ Example(s, Some(body))) => {
	  val result = execute(body)
	  println("  " * a.level + status(result) + " " + s)
	  printMessage(a, result)
	  if (a.lastNode == T) a.copy(lastNode = E)
	  else a.copy(state = Down, lastNode = E)
	}
  }
  private def printMessage(a: Accumulator, result: Result) = {
	result match {
	  case Success(_) => ()
	  case _ => println("  " * (a.level + 1) + result.message)
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
}
trait AConsoleReporter extends AReporter {
  val reporter = new ConsoleReporter {}
}
trait ConsoleOutput extends Output {
  override def println(s: String): Unit = Console.println(s)
}
trait Output {
  def println(s: String): Unit
}