package org.specs2
package reporter

import java.io.StringReader
import scala.xml._
import io._
import main._
import specification._

class JUnitXmlReporterSpec extends Specification { def is =
                                                                                                                        """
The JUnit xml reporter allows to execute specifications and output xml files in a test-reports directory where each xml
is formatted for JUnit reporting tools.
                                                                                                                        """^
 "The output directory"                                                                                                 ^
   "is target/test-reports by default"                                                                                  ! outputDir.e1^
   "can be changed to a user defined directory with -Dspecs2.junit.outputDir"                                           ! outputDir.e1^
                                                                                                                        p^
 "The xml file"                                                                                                         ^
   "must have an outer <testsuite> tag with"                                                                            ^
     "the hostname of the executing machine"                                                                            ! suite().e1^
     "the name of the suite"                                                                                            ! suite().e2^
     "the number of tests"                                                                                              ! suite().e3^
     "the number of errors"                                                                                             ! suite().e4^
     "the number of failures"                                                                                           ! suite().e5^
     "the number of skipped"                                                                                            ! suite().e6^
     "the total time (in seconds)"                                                                                      ! suite().e7^
                                                                                                                        p^
   "must have a <system-out> tag"                                                                                       ! suite().e8^
   "must have a <system-err> tag"                                                                                       ! suite().e9^
                                                                                                                        p^
  "Inside the <testsuite> there is"                                                                                     ^
    "a <properties> tag for all system properties"                                                                      ! suite().e10^
    "a <testcase> tag with"                                                                                             ^
      "the class name"                                                                                                  ! test().e1^
      "the test name"                                                                                                   ! test().e2^
      "the test duration"                                                                                               ! test().e3^
                                                                                                                        p^
  "Inside the <testcase> tag there is"                                                                                  ^
    "the error message"                                                                                                 ! test().e4^
    "the error type"                                                                                                    ! test().e5^
    "the error trace"                                                                                                   ! test().e6^
    "the failure message"                                                                                               ! test().e7^
    "the failure type"                                                                                                  ! test().e8^
    "the failure trace"                                                                                                 ! test().e9^
    "the skipped tag"                                                                                                   ! test().e10^
                                                                                                                        end

  object outputDir {
    val reporter = new JUnitXmlReporter {}

    def e1 = reporter.outputDir must_== "target/test-reports/"
    def e2 = {
      System.setProperty("specs2.junit.outDir", "target/reports/junit")
      reporter.outputDir must_== "target/reports/junit/"
    }
  }

  case class suite() extends WithReporter {
    report("t1" ^
           "e1" ! success^
           "e2" ! anError^
           "e3" ! failure^
           "e4" ! skipped)

    def e1  = xml must \("testsuite", "hostname")
    def e2  = xml must \("testsuite", "name" -> "org.specs2.reporter.JUnitXmlSpecification")
    def e3  = xml must \("testsuite", "tests" -> "4")
    def e4  = xml must \("testsuite", "errors" -> "1")
    def e5  = xml must \("testsuite", "failures" -> "1")
    def e6  = xml must \("testsuite", "skipped" -> "1")
    def e7  = xml must \("testsuite", "time")
    def e8  = xml must \\("system-out")
    def e9  = xml must \\("system-err")
    def e10 = xml must (\\("properties") and \\("property"))
  }

  case class test() extends WithReporter {
    def e1 = xml("t1" ^ "e1" ! success) must \\("testcase", "classname" -> "org.specs2.reporter.JUnitXmlSpecification")
    def e2 = xml("t1" ^ "e1" ! success) must \\("testcase", "name" -> "t1::e1")
    def e3 = xml("t1" ^ "e1" ! success) must \\("testcase", "time")

    def e4 = xml("t1" ^ "e2" ! anError) must \\("error", "message" -> anError.message)
    def e5 = xml("t1" ^ "e2" ! anError) must \\("error", "type" -> anError.exception.getClass.getName)
    def e6 = xml("t1" ^ "e2" ! anError).toString must contain("JUnitXmlReporterSpec.scala")

    def e7 = xml("t1" ^ "e3" ! failure) must \\("failure", "message" -> failure.message)
    def e8 = xml("t1" ^ "e3" ! failure) must \\("failure", "type" -> failure.exception.getClass.getName)
    def e9 = xml("t1" ^ "e3" ! failure).toString must contain("JUnitXmlReporterSpec.scala")

    def e10 = xml("t1" ^ "e2" ! skipped) must \\("skipped")
  }

  trait WithReporter {
    val reporter = new JUnitXmlReporter {
      override lazy val fileWriter = new MockFileWriter {}
    }
    def report(fs: Fragments) = reporter.report(JUnitXmlSpecification(fs))(fullStackTrace)
    def xml(fs: Fragments): NodeSeq = {
      report(fs)
      xml
    }
    def xml: NodeSeq = <out>{scala.xml.XML.load(new StringReader(reporter.fileWriter.getWriter.messages.mkString("\n")))}</out>
  }
}

case class JUnitXmlSpecification(fs: Fragments) extends Specification { def is = fs }