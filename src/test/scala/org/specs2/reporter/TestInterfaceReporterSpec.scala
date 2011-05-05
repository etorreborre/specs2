package org.specs2
package reporter

import org.scalatools.testing.{Logger, EventHandler}
import mock._
import specification.Fragments
import main.Arguments

class TestInterfaceReporterSpec extends Specification { def is =

  "A TestInterfaceReporter should"                                                                                      ^
    "not print the specification title"                                                                                 ! report().e1^
                                                                                                                        end
  case class report() extends Mockito {
    val logger = mock[Logger]
    val handler = mock[EventHandler]

    val reporter = new TestInterfaceReporter(handler, Array(logger))
    def report(fs: =>Fragments, a: Arguments = args(color=false)) = reporter.report(new Specification { def is = fs})(a)

    def e1 = {
      report("title".title ^ "text")
      there was no(logger).info("title")
    }
  }
}