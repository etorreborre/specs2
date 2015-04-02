package org.specs2
package reporter

import examples.{HelloWorldUnitSpec, HelloWorldSpec}
import mock._
import matcher._
import main.Arguments
import io.StringOutput
import sbt.testing._
import runner._
import specification.core._
import specification.process.DefaultExecutor

class SbtPrinterSpec extends Spec with ForEachEnv { def is = s2"""
                                                                                                                        
 A SbtPrinter should
   print the specification title if defined                                   ${printer().e1}
   print HelloWorldSpec ok                                                    ${printer2().e1}

 Sbt event must be fired when a specification is being executed with the SbtPrinter
   TestEvent: succeed                            ${printer().e2}
   the duration must be defined                  ${printer().e3}
   contexts must appear in the name of the event ${printer().e4}
                                                                                 """
  val factory = fragmentFactory; import factory._

  case class printer() extends Mockito { outer =>
    val logger =  mock[Logger]
    val handler = mock[EventHandler]

    val printer = new SbtPrinter {
      lazy val loggers = Array(logger)
      lazy val events = new SbtEvents {
        lazy val handler = outer.handler
        lazy val taskDef = new TaskDef("", Fingerprints.fp1, true, Array())
      }
    }

    def e1 = { env: Env =>
      printer.print(env)(SpecStructure.create(SpecHeader(classOf[HelloWorldSpec]), Fragments(text("\ntitle"), text("\ntext")))).run
      there was one(logger).info(beMatching("HelloWorldSpec\ntitle\ntext"))
    }

    def e2 = { env: Env =>
      executeAndPrintHelloWorldUnitSpec(env)
      there was atLeastOne(handler).handle(eventWithStatus(Status.Success))
    }

    def e3 = { env: Env =>
      executeAndPrintHelloWorldUnitSpec(env)
      there was atLeastOne(handler).handle(eventWithDurationGreaterThanOrEqualTo(0))
    }

    def e4 = { env: Env =>
      executeAndPrintHelloWorldUnitSpec(env)
      there was atLeastOne(handler).handle(eventWithNameMatching("The 'Hello world' string should::contain 11 characters"))
    }

    def eventWithStatus(s: Status): Matcher[Event] =
      beTypedEqualTo(s) ^^ ((_: Event).status())

    def eventWithDurationGreaterThanOrEqualTo(d: Long): Matcher[Event] =
      beGreaterThanOrEqualTo(d) ^^ ((_: Event).duration())

    def eventWithNameMatching(n: String): Matcher[Event] =
      beLike[Selector] { case ts: TestSelector => ts.testName must beMatching(n) } ^^ ((_: Event).selector())

    def executeAndPrintHelloWorldUnitSpec(env: Env) = {
      val executed = DefaultExecutor.executeSpec((new HelloWorldUnitSpec).specificationStructure(env), env)
      printer.print(env)(executed).run
    }

  }

  case class printer2() extends Mockito { outer =>

    def e1 = { env: Env =>
      val hwSpec: org.specs2.Specification = new examples.HelloWorldSpec
      val executed = DefaultExecutor.executeSpec(hwSpec.is, env)

      print(executed).replaceAll("\\d+ ms", "0 ms").replaceAll(" ", "_") ===
      """|HelloWorldSpec
         |
         | This is a specification to check the 'Hello world' string
         |
         | The 'Hello world' string should
         |   + contain 11 characters
         |   + start with 'Hello'
         |   + end with 'world'
         |
         |Total for specification HelloWorldSpec
         |Finished in 0 ms
         |3 examples, 0 failure, 0 error
         |""".stripMargin.replaceAll(" ", "_")
    }

    def print(spec: SpecStructure) = {
      printer.print(Env(arguments = Arguments("nocolor")))(spec).run
      stringLogger.flush()
      stringLogger.messages.mkString("\n")
    }
    val handler = mock[EventHandler]

    val printer = new SbtPrinter {
      lazy val loggers: Array[Logger] = Array(stringLogger)
      lazy val events = new SbtEvents {
        lazy val handler = outer.handler
        lazy val taskDef = new TaskDef("", Fingerprints.fp1, true, Array())
      }
    }

    val stringLogger = new Logger with StringOutput {
      def ansiCodesSupported = false
      def warn(msg: String)  { append(msg) }
      def error(msg: String) { append(msg) }
      def debug(msg: String) { append(msg) }
      def trace(t: Throwable){ append(t.getMessage) }
      def info(msg: String)  { append(msg) }
    }

  }

}

