package org.specs2
package collection

import mutable.Specification
import scala.collection.mutable.*
import scala.concurrent.*, duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import specification.core.*
import org.specs2.concurrent.*
import org.specs2.execute.Result
import org.specs2.control.*
import scala.util.*

class JsExecutionContextSpec extends Specification with CustomJsExecutionContext {
  //args.execute(timeout = 100.millis)

  "A specification can be called with a custom execution context" >> {
    "run an example" >> ok
    "run another example" >> ok
    "the custom execution context must have been called at least twice" >> {
      messages.length must be_>=(2)
    }
    "an example throwing a TimeoutException" >> {
      throw new TimeoutException("out")
      ok
    }
    "an example with a future timing out" >> {
      Future { "start".pp; List.range(1, 2000000).reverse.sorted; "finished".pp; ok }
    }
  }

  def makeFuture(n: Int = 1000000)(using ec: ExecutionContext): Future[Result] =
    if n == 0 then Future(ok) else Future(()).flatMap(_ => makeFuture(n - 1))
}


trait CustomJsExecutionContext extends Specification {
  // this is used to make sure that the provided execution context is really used
  val messages = new ArrayBuffer[String]

  // override the specification args with a specific js execution context
  override def map(structure: SpecStructure): SpecStructure =
    args.execute(jsExecutionContext = executionContext) ^ structure

  // create an execution context wrapping the global execution context
  // but writing a message every time the execute method is called
  lazy val executionContext = new ExecutionContext {
    lazy val global = ExecutionContext.Implicits.global

    def execute(runnable: Runnable): Unit = {
      messages.append("executed a runnable")
      global.execute(runnable)
    }

    def reportFailure(cause: Throwable): Unit =
      global.reportFailure(cause)

  }

}
