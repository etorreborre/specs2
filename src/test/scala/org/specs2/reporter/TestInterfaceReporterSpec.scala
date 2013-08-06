package org.specs2
package reporter

import mock._
import org.specs2.specification.{SpecificationStructure, ExecutingSpecification, Tags, Fragments}
import main.Arguments
import matcher.DataTables
import org.scalatools.testing.{Event, Logger, EventHandler}
import org.specs2.runner.{NullEventHandler, TestInterfaceConsoleReporter}
import scala.annotation.tailrec
import org.specs2.io.StringOutput
import org.specs2.execute.PendingUntilFixed

class TestInterfaceReporterSpec extends Specification with DataTables with PendingUntilFixed { def is = s2"""
                                                                                                                        
  A TestInterfaceReporter should                                                                                      
    print the specification title if defined                                   ${report().e1}
    report datatables ok                                                       ${report().e2}
    print HelloWorldSpec ok                                                    ${report2().e1}
    print HelloWorldSpec with tags ok                                          ${report2().e2}
                                                                               """

  case class report() extends Mockito {
    val logger = mock[Logger]
    val handler = mock[EventHandler]

    val reporter = new TestInterfaceReporter(handler, Array(logger))
    def report(fs: =>Fragments, a: Arguments = args(color=false)) = reporter.report(new Specification { def is = fs})(a)

    def e1 = {
      report("title".title ^ "\ntext")
      there was atLeastOne(logger).info(anyString)
    }
    def e2 = {
      report("ex" ! ("a" | "b" |> 1 ! 2 | { (a, b) => success } ))
      there was atLeastOne(handler).handle(any[Event])
    }
  }

  case class report2() extends TestInterfaceStringReporter {

    def e1 = {
      val hwSpec = new examples.HelloWorldSpec
      stringReport(hwSpec).map(_.replaceAll("\\d+ ms", "0 ms").replaceAll(" ", "_")).mkString("\n") ===
        """|HelloWorldSpec
           |
           | This is a specification to check the 'Hello world' string
           |
           | The 'Hello world' string should
           |   + contain 11 characters
           |   + start with 'Hello'
           |   + end with 'world'
           |_____________________________________________________
           |Total for specification HelloWorldSpec
           |Finished in 0 ms
           |3 examples, 0 failure, 0 error""".stripMargin.replaceAll(" ", "_")
    }
    def e2 = {
      val hwSpec = new HelloWorldSpecWithTags
      stringReport(hwSpec).map(_.replaceAll("\\d+ ms", "0 ms").replaceAll(" ", "_")).mkString("\n") ===
        """|HelloWorldSpecWithTags
           |
           | This is a specification to check the 'Hello world' string
           |
           | The 'Hello world' string should
           |   + contain 11 characters_
           |   + start with 'Hello'_
           |   + end with 'world'
           |_____________________________________________________
           |Total for specification HelloWorldSpecWithTags
           |Finished in 0 ms
           |3 examples, 0 failure, 0 error""".stripMargin.replaceAll(" ", "_")
    }
  }

}

class HelloWorldSpecWithTags extends Specification { def is = s2"""

 This is a specification to check the 'Hello world' string

 The 'Hello world' string should
   contain 11 characters                             $e1 ${tag("x")}
   start with 'Hello'                                $e2 ${section("y")}
   end with 'world'                                  $e3
                                                     """

  def e1 = "Hello world" must have size(11)
  def e2 = "Hello world" must startWith("Hello")
  def e3 = "Hello world" must endWith("world")

}


trait TestInterfaceStringReporter {
  def stringReport(s: SpecificationStructure) = {

    val stringLogger = new Logger with StringOutput {
      def ansiCodesSupported = false
      def warn(msg: String)  { append(msg) }
      def error(msg: String) { append(msg) }
      def debug(msg: String) { append(msg) }
      def trace(t: Throwable){ append(t.getMessage) }
      def info(msg: String)  { append(msg) }
    }
    val reporter = new TestInterfaceConsoleReporter(Some(new TestInterfaceReporter(NullEventHandler, Array(stringLogger))), (a: Arguments) => Seq())
    reporter.report(s)(Arguments("nocolor"))
    stringLogger.messages
  }
}
