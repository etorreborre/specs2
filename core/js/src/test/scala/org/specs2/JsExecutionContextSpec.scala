package org.specs2
package collection

import mutable.Specification
import scala.collection.mutable._
import scala.concurrent._
import specification.core._

class JsExecutionContextSpec extends Specification with CustomJsExecutionContext {

  "A specification can be called with a custom execution context" >> {
    "run an example" >> ok
    "run another example" >> ok
    "the custom execution context must have been called at least twice" >> {
      messages.length must be_>=(2)
    }
  }

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
