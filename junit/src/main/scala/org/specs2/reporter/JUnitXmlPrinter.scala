package org.specs2
package reporter

import java.io.Writer
import java.net.InetAddress
import scala.xml.{XML, NodeSeq}
import org.junit.runner.Description
import scala.collection.JavaConversions._
import control.Exceptions._
import xml.Nodex._
import execute._
import main.Arguments
import io.Location
import specification._


/**
 * The JUnitXml printer is used to create a JUnit xml report of an executed specification.
 *
 * To do this, it uses a reducer to prepare print blocks with:
 *
 * - the text to print
 * - the statistics
 * - the current arguments to use
 *
 */
trait JUnitXmlPrinter {

  /**
   * create a TestSuite object containing all the examples
   */
  def testSuite(name: SpecName, fs: Seq[ExecutedFragment])(implicit args: Arguments) = {
    /** extract the root Description object and the examples to execute */
    lazy val DescriptionAndExamples(desc, executions) = descriptions(name).foldAll(fs)
    lazy val statistics: Stats = fs.headOption match {
      case Some(s @ ExecutedSpecStart(_,_,_)) => s.stats
      case _                                  => Stats()
    }
    lazy val start = TestSuite(desc, name.javaClassName, statistics.errors, statistics.failures, statistics.skipped, statistics.timer.totalMillis)

    executions.foldLeft(start) { (suite, de) =>
      val (f, d) = de

      // temporary fix for http://bit.ly/WDGAT8 where a pending text might be defined as a test case
      // in that case the method name is null which triggers a parse error for bamboo because the name attribute is
      // missing
      if (d.isTest && Option(d.getMethodName).isDefined) suite.addTest(TestCase(d, f))
      else                                               suite
    }
  }

  /** fold object used to create descriptions */
  def descriptions(name: SpecName)(implicit args: Arguments) = new JUnitDescriptions[ExecutedFragment](name.javaClassName)(Levels.ExecutedLevelsReducer) {
    def initialFragment(className: String) = ExecutedText(Text(className), new Location())
    /**
     * This function is used to map each node in a Tree[Fragment] to a pair of
     * (Description, Fragment)
     *
     * The Int argument is the numeric label of the current TreeNode being mapped.
     * It is used to create a unique description of the example to executed which is required
     * by JUnit
     */
    def mapper(className: String): (ExecutedFragment, Seq[DescribedFragment], Int) => Option[DescribedFragment] =
      (f: ExecutedFragment, parentNodes: Seq[DescribedFragment], nodeLabel: Int) => f match {
        case s @ ExecutedSpecStart(_,_,_)             => Some(f -> createDescription(className, suiteName=testName(s.name)))
        case ExecutedText(t,_) if t.t.trim.nonEmpty   => Some(f -> createDescription(className, suiteName=testName(t.t)))
        case r @ ExecutedResult(_,_,_,_,_)            => Some(f -> createDescription(className, label=nodeLabel.toString, testName=testName(r.text.toString, parentPath(parentNodes))) )
        case other                                    => None
      }
  }

  private def formatTime(t: Long) = "%.3f" format (t / 1000.0)

  case class TestSuite(description: Description, className: String, errors: Int, failures: Int, skipped: Int, time: Long = 0, tests: Seq[TestCase] = Seq())(implicit args: Arguments) {
    def addTest(t: TestCase) = copy(tests = tests :+ t)
    def flush(out: Writer) = XML.write(out, xml, "utf-8", false, null)

    def xml =
      <testsuite hostname={tryo(InetAddress.getLocalHost.getHostName).getOrElse("no host detected")}
                 name={className}
                 tests={tests.size.toString}
                 errors={errors.toString}
                 failures={failures.toString}
                 skipped={skipped.toString}
                 time={formatTime(time)}>
        {properties}
        {tests.map(_.xml).reduceNodes}
        <system-out><![CDATA[]]></system-out>
        <system-err><![CDATA[]]></system-err>
      </testsuite>

    def properties =
      <properties>
      {System.getProperties.entrySet.toSeq.map(p => <property name={p.getKey.toString} value={p.getValue.toString}/>).reduceNodes}
      </properties>
  }

  case class TestCase(desc: Description, fragment: ExecutedFragment)(implicit args: Arguments) {
    def xml =
      <testcase name={desc.getMethodName} classname={desc.getClassName} time={formatTime(time)}>
        {testError}{testFailure}{testSkipped}{testPending}
      </testcase>

    def time = fragment match {
      case ExecutedResult(_,_,t,_,_) => t.totalMillis
      case other                     => 0
    }

    def testError = fragment match {
      case ExecutedResult(_,er @ Error(m, e),_,_,_) => <error message={m}
                                                            type={e.getClass.getName}>{args.traceFilter(er.stackTrace).mkString("\n")}</error>
      case other                                  => NodeSeq.Empty
    }

    def testFailure = fragment match {
      case ExecutedResult(_,f @ Failure(m, e, st, d),_,_,_) => <failure message={m}
                                                                      type={f.exception.getClass.getName}>{args.traceFilter(st).mkString("\n")}</failure>
      case other                                           => NodeSeq.Empty
    }

    def testPending = fragment match {
      case ExecutedResult(_, Pending(m),_,_,_) => <skipped/>
      case other                                  => NodeSeq.Empty
    }

    def testSkipped = fragment match {
      case ExecutedResult(_, Skipped(m, e),_,_,_) => <skipped/>
      case other                                  => NodeSeq.Empty
    }
  }

}
