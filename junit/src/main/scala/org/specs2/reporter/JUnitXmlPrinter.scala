package org.specs2
package reporter

import foldm._, FoldM._, FoldableM._, stream._, FoldProcessM._
import scalaz.{Failure => _, _}, Scalaz._
import org.junit.runner.Description
import java.net.InetAddress
import main.Arguments
import execute._
import io.FileName
import scalaz.concurrent.Task
import control._
import io._
import scala.collection.JavaConversions._
import Exceptions._
import specification.core._
import specification.process._
import text.NotNullStrings._
import JUnitDescriptions._

/**
 * The JUnitXmlPrinter creates an xml file with the specification execution results
 */
trait JUnitXmlPrinter extends Printer {
  def prepare(env: Env, specs: List[SpecStructure]): Action[Unit]  = Actions.unit
  def finalize(env: Env, specs: List[SpecStructure]): Action[Unit] = Actions.unit

  def sink(env: Env, spec: SpecStructure): AsyncSink[Fragment] =
    (Statistics.fold zip FoldId.list[Fragment]).into[Task].
      mapFlatten(saveResults(env, spec))

  def saveResults(env: Env, spec: SpecStructure): ((Stats, List[Fragment])) =>  Task[Unit] = { case (stats, fs) =>
    val suite = descriptionFold(spec, stats, env).run(descriptions(spec, fs).toList)
    env.fileSystem.writeFileTask(outputDirectory(env) | FileName.unsafe(spec.specClassName+".xml"), suite.xml)
  }

  def descriptionFold(spec: SpecStructure, stats: Stats, env: Env): Fold[(Fragment, Description), TestSuite] = {
    val suite = TestSuite(specDescription(spec), spec.specClassName, stats.errors, stats.failures, stats.skipped, stats.timer.totalMillis)
    fromFoldLeft[(Fragment, Description), TestSuite](suite) { case (res, (f, d)) =>
      if (Fragment.isExample(f)) res.addTest(new TestCase(d, f.executionResult, f.execution.executionTime.totalMillis)(env.arguments))
      else                       res
    }
  }

  def descriptions(spec: SpecStructure, fragments: List[Fragment]) =
    JUnitDescriptions.fragmentDescriptions(spec.setFragments(Fragments(fragments:_*)))

  def outputDirectory(env: Env): DirectoryPath =
    env.arguments.commandLine.directoryOr("junit.outdir", "target" / "test-reports")

  case class TestSuite(description: Description, className: String, errors: Int, failures: Int, skipped: Int, time: Long = 0, tests: Seq[TestCase] = Seq()) {
    def addTest(t: TestCase) = copy(tests = tests :+ t)

    def xml =
      s"""|<?xml version='1.0' encoding='utf-8'?>
          |<testsuite hostname="${tryo(InetAddress.getLocalHost.getHostName).getOrElse("no host detected")}"
          |           name="$className"
          |           tests="${tests.size.toString}"
          |           errors="${errors.toString}"
          |           failures="${failures.toString}"
          |           skipped="${skipped.toString}"
          |           time="${formatTime(time)}">
          |  $properties
          |  ${tests.map(_.xml).mkString("\n")}
          |  <system-out><![CDATA[]]></system-out>
          |  <system-err><![CDATA[]]></system-err>
          |</testsuite>""".stripMargin

    /**
     * output properties
     */
    def properties =
      s"""<properties>
            ${System.getProperties.entrySet.toSeq.map(p => s"""<property name="${escape(p.getKey.toString)}" value="${escape(p.getValue.toString)}" ></property>""").mkString("\n")}
          </properties>"""
  }

  case class TestCase(desc: Description, result: Result, time: Long)(implicit args: Arguments) {
    def xml =
      s"""<testcase name="${escape(desc.getMethodName)}" classname="${escape(desc.getClassName)}" time="${escape(formatTime(time))}">
            $testError$testFailure$testSkipped$testPending
          </testcase>"""

    def testError = result match {
      case er @ Error(m, e) => s"""<error message="${escape(m)}" type="${escape(e.getClass.getName)}">${escape(args.traceFilter(er.stackTrace).mkString("\n"))}</error>"""
      case _ => ""
    }

    def testFailure = result match {
      case f @ Failure(m, e, st, d) => s"""<failure message="${escape(m)}" type="${escape(f.exception.getClass.getName)}">${escape(args.traceFilter(st).mkString("\n"))}</failure>"""
      case _ => ""
    }

    def testPending = result match {
      case Pending(m) => "<skipped/>"
      case _ => ""
    }

    def testSkipped = result match {
      case Skipped(m, e) => """<skipped/>"""
      case _ => ""
    }
  }

  private def escape(s: =>String): String =
    scala.xml.Utility.escape(s.notNull)

  private def formatTime(t: Long) = "%.3f" format (t / 1000.0)
}

object JUnitXmlPrinter extends JUnitXmlPrinter
