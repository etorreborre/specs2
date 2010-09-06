package org.specs2.runner
import org.specs2.Specification
import org.specs2.specification._

trait ConsoleReporter extends Reporter[Unit] with TreeExamplesParser with ConsoleOutput with ExampleExecution {
  val mapper: PartialFunction[Fragment, Unit] = {
	case Text(s) => println(s)
	case e @ Example(s, Some(body)) => {
	  val result = execute(body)
	  println(s)
	}
  }
}
trait AConsoleReporter extends AReporter[Unit] {
  val reporter = new ConsoleReporter {}
}
trait ConsoleOutput {
  def println(s: String): Unit = Console.println(s)
}
trait Output {
  def println(s: String): Unit
}