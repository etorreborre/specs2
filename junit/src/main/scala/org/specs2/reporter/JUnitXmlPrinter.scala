package org.specs2
package reporter

import org.junit.runner.Description
import java.net.InetAddress
import main.Arguments
import execute._
import data.Fold
import io.FileName
import scalaz.concurrent.Task
import control._
import io._
import scala.collection.JavaConversions._
import Exceptions._
import specification.core._
import specification.process._

/**
 * The JUnitXmlPrinter creates an xml file with the specification execution results
 */
trait JUnitXmlPrinter extends Printer {
  def prepare(env: Env, specs: List[SpecificationStructure]): Action[Unit]  = Actions.unit
  def finalize(env: Env, specs: List[SpecificationStructure]): Action[Unit] = Actions.unit

  def fold(env: Env, spec: SpecStructure): Fold[Fragment] = new Fold[Fragment] {
    type S = Stats

    private val description = JUnitDescriptions.specDescription(spec)
    private val descriptions = JUnitDescriptions.fragmentDescriptions(spec)

    lazy val sink = Fold.unitSink[Fragment, Stats]

    def prepare = Task.now(())
    def fold = Statistics.fold
    def init = Stats.empty

    def last(stats: Stats) = {
      val start = TestSuite(description, spec.specClassName, stats.errors, stats.failures, stats.skipped, stats.timer.totalMillis)
      val suite = descriptions.foldLeft(start) { case (res, (f, d)) =>
        res.addTest(new TestCase(d, f.executionResult, f.execution.executionTime.totalMillis)(env.arguments))
      }
      val outputDirectory = env.arguments.commandLine.directoryOr("junit.outdir", "target" / "test-reports")
      env.fileSystem.writeFileTask(outputDirectory | FileName.unsafe(spec.specClassName+".xml"), suite.xml)
    }

  }

  case class TestSuite(description: Description, className: String, errors: Int, failures: Int, skipped: Int, time: Long = 0, tests: Seq[TestCase] = Seq()) {
    def addTest(t: TestCase) = copy(tests = tests :+ t)

    def xml =
      s"""<testsuite hostname=${tryo(InetAddress.getLocalHost.getHostName).getOrElse("no host detected")}
                     name=$className
                     tests=${tests.size.toString}
                     errors=${errors.toString}
                     failures=${failures.toString}
                     skipped=${skipped.toString}
                     time=${formatTime(time)}>
        $properties
        ${tests.map(_.xml).mkString("\n")}
        <system-out><![CDATA[]]></system-out>
        <system-err><![CDATA[]]></system-err>
      </testsuite>"""

    def properties =
      s"""<properties>
            ${System.getProperties.entrySet.toSeq.map(p => s"""<property name={p.getKey.toString} value={p.getValue.toString}/>""").mkString("\n")}
          </properties>"""
  }

  case class TestCase(desc: Description, result: Result, time: Long)(implicit args: Arguments) {
    def xml =
      s"""<testcase name=${desc.getMethodName} classname=${desc.getClassName} time=${formatTime(time)}>
           ${testError}${testFailure}${testSkipped}$testPending
         </testcase>"""

    def testError = result match {
      case er @ Error(m, e) => s"""<error message=s{m} type=${e.getClass.getName}>${args.traceFilter(er.stackTrace).mkString("\n")}</error>"""
      case _ => ""
    }

    def testFailure = result match {
      case Failure(m, e, st, d) => s"""<failure message=$m type=${e.getClass.getName}>${args.traceFilter(st).mkString("\n")}</failure>"""
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

  private def formatTime(t: Long) = "%.3f" format (t / 1000.0)
}

object JUnitXmlPrinter extends JUnitXmlPrinter
