package org.specs2
package reporter

import org.junit.runner.Description
import java.net.InetAddress

import main.Arguments
import execute.*
import fp.syntax.*
import io.FileName
import io.FileName.ToFileName
import control.*
import io.*

import scala.collection.JavaConverters.*
import Exceptions.*
import concurrent.ExecutionEnv
import specification.core.*
import specification.process.*
import text.NotNullStrings.*
import JUnitDescriptions.*
import origami.*
import Folds.*

/** The JUnitXmlPrinter creates an xml file with the specification execution results
  */
case class JUnitXmlPrinter(env: Env) extends Printer:
  def prepare(specs: List[SpecStructure]): Action[Unit] = Action.unit
  def finalize(specs: List[SpecStructure]): Action[Unit] = Action.unit

  def sink(spec: SpecStructure): AsyncSink[Fragment] =
    (Statistics.fold `zip` list[Fragment].into[Action]).mapFlatten(saveResults(spec))

  def saveResults(spec: SpecStructure): ((Stats, List[Fragment])) => Action[Unit] = { case (stats, fs) =>
    descriptionFold(spec, stats).run(descriptions(spec, fs)(env.specs2ExecutionEnv).toList).flatMap { suite =>
      env.fileSystem
        .writeFile(outputDirectory(env.arguments) | FileName.unsafe(spec.specClassName + ".xml"), suite.xml)
        .toAction
    }
  }

  def descriptionFold(spec: SpecStructure, stats: Stats): AsyncFold[(Fragment, Description), TestSuite] =
    val suite = TestSuite(
      specDescription(spec),
      spec.specClassName,
      stats.errors,
      stats.failures,
      stats.skipped,
      stats.timer.totalMillis
    )
    fromFoldLeft[Action, (Fragment, Description), TestSuite](suite) { case (res, (f, d)) =>
      if Fragment.isExample(f) then
        f.executedResult.map { case ExecutedResult(result, timer) =>
          res.addTest(new TestCase(d, result, timer.totalMillis)(using env.arguments))
        }
      else Action.pure(res)
    }

  def descriptions(spec: SpecStructure, fragments: List[Fragment])(ee: ExecutionEnv) =
    JUnitDescriptions.fragmentDescriptions(spec.setFragments(Fragments(fragments*)))(ee)

  def outputDirectory(arguments: Arguments): DirectoryPath =
    arguments.commandLine.directoryOr("junit.outdir", "target" / "test-reports")

  case class TestSuite(
      description: Description,
      className: String,
      errors: Int,
      failures: Int,
      skipped: Int,
      time: Long = 0,
      tests: Seq[TestCase] = Seq()
  ):
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

    /** output properties
      */
    def properties =
      s"""<properties>
            ${System.getProperties.entrySet.asScala.toSeq
        .map(p =>
          s"""<property name="${escape(p.getKey.toString)}" value="${escape(p.getValue.toString)}" ></property>"""
        )
        .mkString("\n")}
          </properties>"""

  case class TestCase(desc: Description, result: Result, time: Long)(using args: Arguments):
    def xml =
      s"""<testcase name="${escape(desc.getMethodName)}" classname="${escape(desc.getClassName)}" time="${escape(
        formatTime(time)
      )}">
            $testError$testFailure$testSkipped$testPending
          </testcase>"""

    def testError = result match
      case er @ Error(m, e) =>
        s"""<error message="${escape(m)}" type="${escape(e.getClass.getName)}">${escape(
          args.traceFilter(er.stackTrace).mkString("\n")
        )}</error>"""
      case _ => ""

    def testFailure = result match
      case f @ Failure(m, e, st, d) =>
        s"""<failure message="${escape(m)}" type="${escape(f.exception.getClass.getName)}">${escape(
          args.traceFilter(st).mkString("\n")
        )}</failure>"""
      case _ => ""

    def testPending = result match
      case Pending(m) => "<skipped/>"
      case _          => ""

    def testSkipped = result match
      case Skipped(m, e) => """<skipped/>"""
      case _             => ""

  private def escape(s: =>String): String =
    scala.xml.Utility.escape(s.notNull)

  private def formatTime(t: Long) = "%.3f" format (t / 1000.0)
