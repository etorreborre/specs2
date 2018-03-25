package org.specs2
package reporter

import io._
import specification.core._

import scala.xml.NodeSeq
import matcher._
import control._
import ExecuteActions._
import org.specs2.main.Arguments

class JUnitXmlPrinterSpec(val env: Env) extends Specification with XmlMatchers with OwnEnv { def is = s2"""

The JUnitXmlPrinter allows to execute specifications and output xml files in a test-reports directory where each xml
is formatted for JUnit reporting tools.

 The output directory
   is target/test-reports by default                                                                                    ${outputDir.e1}
   can be changed to a user defined directory with -Dspecs2.junit.outputDir                                             ${outputDir.e1}

 The xml file
   must have a xml header                                                                                               $header
   must have an outer <testsuite> tag with
     the hostname of the executing machine                                                                              ${suite.e1}
     the name of the suite                                                                                              ${suite.e2}
     the number of tests                                                                                                ${suite.e3}
     the number of errors                                                                                               ${suite.e4}
     the number of failures                                                                                             ${suite.e5}
     the number of skipped                                                                                              ${suite.e6}
     the total time (in seconds)                                                                                        ${suite.e7}

   must have a <system-out> tag                                                                                         ${suite.e8}
   must have a <system-err> tag                                                                                         ${suite.e9}

  Inside the <testsuite> there is
    a <properties> tag for all system properties                                                                        ${suite.e10}
    a <testcase> tag with
      the class name                                                                                                    ${test.e1}
      the test name                                                                                                     ${test.e2}
      the test duration                                                                                                 ${test.e3}

  Inside the <testcase> tag there is
    the error message                                                                                                   ${message.e1}
    the error type                                                                                                      ${message.e2}
    the error trace                                                                                                     ${message.e3}
    the failure message                                                                                                 ${message.e4}
    the failure type                                                                                                    ${message.e5}
    the failure trace                                                                                                   ${message.e6}
    the skipped tag                                                                                                     ${message.e7}
    names must be escaped                                                                                               ${message.e8}
  """

  val printer = JUnitXmlPrinter

  object outputDir {

    def e1 = {
      printer.outputDirectory(Arguments()).path must endWith("target/test-reports")
    }

    def e2 = {
      System.setProperty("specs2.junit.outDir", "target/reports/junit")
      printer.outputDirectory(Arguments()).path must endWith("target/reports/junit")
    }
  }

  def header = { printString(ownEnv)("e1" ! success) must startWith("<?xml version='1.0' encoding='utf-8'?>") }

  object suite {
    def xml(env: Env) =
      print(env)("t1" ^ br ^
      "e1<>&\"" ! success^ br^    // for testing, this name includes special characters that are escaped in xml
      "e2"      ! anError^ br^
      "e3"      ! failure^ br^
      "e4"      ! skipped)

    def e1  = { xml(ownEnv) must \\("testsuite", "hostname") }
    def e2  = { xml(ownEnv) must \\("testsuite", "name" -> "org.specs2.reporter.JUnitXmlPrinterSpec") }
    def e3  = { xml(ownEnv) must \\("testsuite", "tests" -> "4") }
    def e4  = { xml(ownEnv) must \\("testsuite", "errors" -> "1") }
    def e5  = { xml(ownEnv) must \\("testsuite", "failures" -> "1") }
    def e6  = { xml(ownEnv) must \\("testsuite", "skipped" -> "1") }
    def e7  = { xml(ownEnv) must \\("testsuite", "time") }
    def e8  = { xml(ownEnv) must \\("system-out") }
    def e9  = { xml(ownEnv) must \\("system-err") }
    def e10 = { xml(ownEnv) must (\\("properties") and \\("property")) }
  }

  object test {
    def e1 = { print(ownEnv)("t1" ^ br ^ "e1<>&\"" ! success) must \\("testcase", "classname" -> "org.specs2.reporter.JUnitXmlPrinterSpec") }
    def e2 = { print(ownEnv)("t1" ^ br ^ "e1<>&\"" ! success) must \\("testcase", "name" -> scala.xml.Utility.escape("t1::e1<>&\"")) }
    def e3 = { print(ownEnv)("t1" ^ br ^ "e1<>&\"" ! success) must \\("testcase", "time") }
  }

  object message {
    def e1 = { print(ownEnv)("t1" ^ br ^ "e2" ! anError) must \\("error", "message" -> anError.exception.getMessage) }
    def e2 = { print(ownEnv)("t1" ^ br ^ "e2" ! anError) must \\("error", "type" -> anError.exception.getClass.getName) }
    def e3 = { print(ownEnv)("t1" ^ br ^ "e2" ! anError).toString must contain("JUnitXmlPrinterSpec.scala") }
    def e4 = { print(ownEnv)("t1" ^ br ^ "e3" ! failure) must \\("failure", "message" -> failure.message) }
    def e5 = { print(ownEnv)("t1" ^ br ^ "e3" ! failure) must \\("failure", "type" -> failure.exception.getClass.getName) }
    def e6 = { print(ownEnv)("t1" ^ br ^ "e3" ! failure).toString must contain("JUnitXmlPrinterSpec.scala") }
    def e7 = { print(ownEnv)("t1" ^ br ^ "e2" ! skipped) must \\("skipped") }
    def e8 = { print(ownEnv)("t1" ^ br ^ "<node.1/>" ! ok).toString must contain("t1::&lt;node.1/&gt;") }
  }

  def printString(env1: Env)(fs: Fragments): String = {
    val mockFs = new FileSystem {
      var out: String = ""
      override def writeFile(filePath: FilePath, content: String): Operation[Unit] =
        Operations.ok(this.out = content)
    }
    val env = env1.copy(fileSystem = mockFs)
    Reporter.report(env, List(JUnitXmlPrinter))(SpecStructure(SpecHeader(getClass)).setFragments(fs)).runOption(env1.specs2ExecutionEnv)
    mockFs.out
  }

  def print(env: Env)(fs: Fragments): NodeSeq =
    scala.xml.XML.loadString(printString(env)(fs))
}

case class JUnitXmlSpecification(fs: Fragments) extends Specification { def is = fs }
