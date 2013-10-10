package org.specs2
package runner

import io._
import mock.Mockito
import matcher.DataTables
import reporter._
import org.scalatools.testing._
import main.{ArgumentsArgs, Arguments}
import specification.{Tags, Groups, ExecutingSpecification}
import matcher._
import execute.StandardResults
import scala.collection.mutable.ListBuffer
import annotation.tailrec

class TestInterfaceRunnerSpec extends Specification with Groups with Tags { def is = section("unstable") ^ s2"""
                                                                                                                
 A TestInterfaceRunner is responsible for instantiating Specification classes found by
 sbt and executing them using a TestInterfaceReporter


 if the specification class is missing
   there must be an error logged                                                                              ${g1().e1}
   a stacktrace must be logged                                                                                ${g1().e2}

 if the specification instance cannot be created
   a stacktrace for the exception must be logged                                                              ${g2().e1}
   the cause stacktrace must also be logged if there is one                                                   ${g2().e2}
   the cause stacktrace must be nicely separated from the top exception                                       ${g2().e3}

 if the specification instance can be created it must be passed to TestInterfaceReporter                      ${g3().e1}
 Additional report types can be passed on the command line                                                    ${g3().e2}
 A custom notifier can be specified on the command line with 'notifier <class name>'                          ${g3().e3}
 A custom exporter can be specified on the command line with 'exporter <class name>'                          ${g3().e4}
 A custom notifier can be specified in the specification with 'args.report(exporter=<class name>)'            ${g3().e5}
 A custom exporter can be specified in the specification with 'args.report(exporter=<class name>)'            ${g3().e6}

 Execution
   if the results are displayed on the console
     the storing must be done in parallel to the exporting (to get results asap on the console)               ${g4().e1}
     the other exporters must use the result of the storing                                                   ${g4().e2}
   The handler must get test names for failures and errors                                                    ${g5().e1}
                                                                                                              """

  "missing" - new g1 {
    object run extends MockLogger {
      val runner = new TestInterfaceRunner(getClass.getClassLoader, Array(logger))
      runner.run("missing", mock[TestFingerprint], mock[EventHandler], Array(""))
    }
    e1 := run.logger.messages must contain("error: Could not create an instance of missing\n")
    e2 := run.logger.messages must contain("error:   caused by java.lang.ClassNotFoundException: missing")
  }

  "instance" - new g2 {
    object run extends MockLogger {
      val runner = new TestInterfaceRunner(getClass.getClassLoader, Array(logger))
      runner.run("org.specs2.runner.SpecificationForSbtWithException", mock[TestFingerprint], mock[EventHandler], Array(""))
    }
    e1 := run.logger.messages must
         contain("error: Could not create an instance of org.specs2.runner.SpecificationForSbtWithException\n")
    e2 := run.logger.messages must
          contain("error:   caused by java.lang.IllegalArgumentException: cause")
    e3 := run.logger.messages must
          contain("error:   caused by java.lang.Exception: fail")
  }

  "reporting" - new g3 with Mockito with MockLogger with DataTables with matcher.MustMatchers with ArgumentsArgs {
    val outer = this
    val reporter = mock[Reporter]
    val handler = mock[EventHandler]

    val runner = new TestInterfaceRunner(getClass.getClassLoader, Array(logger)) {
      override def reporter(handler: EventHandler)(args: Array[String]): Reporter = outer.reporter
    }

    def reportSpec(args: Array[String] = Array("")) =
      runner.run("org.specs2.runner.SpecificationForSbt", mock[TestFingerprint], mock[EventHandler], args)

    e1 := {
      reportSpec()
      there was one(reporter).report(any[specification.SpecificationStructure])(any[Arguments])
    }

    e2 := {
      implicit val args = Arguments()

      def export(condition: Boolean, e: String) = if (condition) Some(e) else None
      def selectedExporters(c: Boolean, h: Boolean, m: Boolean, j: Boolean) =
        Seq(export(c, "TestInterfaceReporter"), export(h, "HtmlExporting$"), export(m, "MarkdownExporting$"), export(j, "JUnitXmlExporting$")).flatten

      "args"                                  || "console" | "html" | "markdown"  | "junitxml" |
      "junitxml"                              !! false     ! false  ! false       ! true       |
      "junitxml,console"                      !! true      ! false  ! false       ! true       |
      "junitxml,html,console"                 !! true      ! true   ! false       ! true       |
      "junitxml,markdown,console"             !! true      ! false  ! true        ! true       |> { (arguments, c, h, m, j) =>
        runner.exporters(arguments.split(","), handler).map(_.getClass.getSimpleName) must contain(allOf(selectedExporters(c, h, m, j):_*))
      }

    }

    e3 := {
      val args = Array("notifier", "user.reporter.CustomNotifier")
      atLeastOnce(runner.exporters(args, handler)(Arguments(args:_*))) { e =>
        e must haveInterface[NotifierExporting]
      }
    }

    e4 := {
      val args = Array("exporter", "user.reporter.CustomExporter")
      atLeastOnce(runner.exporters(args, handler)(Arguments(args:_*))) { e =>
        e must haveInterface[Exporter]
      }
    }

    e5 := {
      atLeastOnce(runner.exporters(Array[String](), handler)(args.report(notifier="user.reporter.CustomNotifier"))) { e =>
        e must haveInterface[NotifierExporting]
      }
    }

    e6 := {
      atLeastOnce(runner.exporters(Array[String](), handler)(args.report(exporter="user.reporter.CustomExporter"))) { e =>
        e must haveInterface[Exporter]
      }
    }
  }

  "executing" - new g4 with  matcher.MustMatchers with ArgumentsArgs with StandardResults with StringOutput {
    val htmlExporter = new HtmlExporting { override def export(implicit args: Arguments) = (spec: ExecutingSpecification) =>
      { println("html export"); spec.execute }
    }
    val testInterfaceReporter = new TestInterfaceReporter(NullEventHandler, Array()) {
      override def export(implicit args: Arguments) = (spec: ExecutingSpecification) => {
        val executed = spec.execute
        println("console export")
        executed
      }
    }

    val reporter = new TestInterfaceConsoleReporter(Some(testInterfaceReporter), (a: Arguments) => Seq(htmlExporter)) {
      override def store(implicit args: Arguments) = (spec: ExecutingSpecification) => {
        // take some time to do the storing
        @tailrec
        def loop(i: Int): Unit = if (i == 0) () else loop(i - 1)
        loop(100000)
        println("stored"); spec
      }
    }

    val spec = new Specification { def is = ok }
    e1 := {
      // out of 10 tries we should see the storing happening after the exporting
      val results = new ListBuffer[Seq[String]]
      (1 to 10).foreach { i =>
        reporter.report(spec)(Arguments("console"))
        results.append(messages)
        clear()
      }
      atLeastOnce(results)((msgs: Seq[String]) => msgs aka results.mkString("\n") must contain(allOf("console export", "stored")).inOrder)
    }
    e2 := {
      reporter.report(spec)(Arguments("console html"))
      messages must contain(allOf("stored", "html export")).inOrder
    }
  }

  "events" - new g5 with Mockito with MockLogger with matcher.MustMatchers with ArgumentsArgs {
    val outer = this
    val handler = mock[EventHandler]
    val reporter = new TestInterfaceReporter(handler, Array())
    e1 := {
      reporter.report(new Specification { def is = "test1" ! ko })(Arguments())
      there was one(handler).handle(eventWithName("test1"))
    }

    def eventWithName(name: String): Matcher[Event] =
      (e: Event) => (e.testName == name, s"${e.testName} != $name")
  }
}

trait MockLogger extends matcher.MustExpectations with Mockito {
  val logger = new Logger with StringOutput {
    override def ansiCodesSupported = false
    override def error(message: String) = println("error: " + message)
    override def info(message: String)  = println("info: " + message)
    override def warn(message: String)  = println("warn: " + message)
    override def debug(message: String) = println("debug: " + message)
    override def trace(t: Throwable)    = println("trace: " + t)
  }
}

class SpecificationForSbtWithException extends Specification {
  val cause = new IllegalArgumentException("cause")
  throw new Exception("fail", cause)
  def is = "ex1" ! success ^ end
}
class SpecificationForSbt extends Specification {
  def is = "ex1" ! success ^ end
}
