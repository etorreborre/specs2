package org.specs2.runner
import org.specs2.Specification
import org.specs2.specification._

trait ConsoleReporter extends Reporter with ConsoleOutput with ExampleExecution {
  case class Accumulator(indent: String = "")
  type T = Accumulator
  val initial = new Accumulator()
  val folder: PartialFunction[(Accumulator, Fragment), Accumulator] = {
	case (a, Text(s)) => println(a.indent + s); a.copy(indent = a.indent + "  ")
	case (a, e @ Example(s, Some(body))) => {
	  val result = execute(body)
	  println(a.indent + s)
	  a
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