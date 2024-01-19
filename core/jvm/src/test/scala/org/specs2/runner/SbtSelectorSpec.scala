package org.specs2
package runner

import sbt.testing.{Event, EventHandler, Logger, Selector, Status, SuiteSelector, TaskDef, TestSelector}

import scala.collection.mutable.ArrayBuffer

class SbtSelectorSpec extends Specification:
  def is = s2"""

 An sbt runner executes the examples passed in the `TestSelector`s
   when there is a single `TestSelector`       $singleTestSelector
   when there are 2 `TestSelector`s            $twoTestSelectors
   run everything if there are other selectors $otherSelectors
   run nothing if there are no matches         $noMatches
   regexes in test selectors are escaped       $regexesAreEscaped
"""

  private def singleTestSelector =
    val wholeSuiteEvents = runWithSelectors(new SuiteSelector :: Nil)
    wholeSuiteEvents.length === 3
    val failEvents = wholeSuiteEvents.filter(_.status == Status.Failure)
    val singleExampleEvents = runWithSelectors(failEvents.map(_.selector()))

    (failEvents must haveSize(1)) and
      (testName(failEvents.head) === "has a failing test") and
      (singleExampleEvents must haveSize(1)) and
      (singleExampleEvents.head.status() === Status.Failure)

  private def twoTestSelectors =
    val events = runWithSelectors(
      List(new TestSelector("has a successful test"), new TestSelector("has a failing test"))
    )
    (events must haveSize(2)) and
      (events.map(testName) must contain("has a successful test", "has a failing test"))

  private def otherSelectors =
    val events = runWithSelectors(List(new SuiteSelector, new TestSelector("hello")))
    (events must haveSize(3)) and
      (events.map(testName) must contain("has a successful test", "has a failing test", ".*"))

  private def noMatches =
    val events = runWithSelectors(List(new TestSelector("won't match anything")))
    events must beEmpty

  private def regexesAreEscaped =
    val events = runWithSelectors(new TestSelector(".*") :: Nil)
    (events must haveSize(1)) and
      (events.head.status() === Status.Success) and
      (testName(events.head) === ".*")

  private def runWithSelectors(selectors: List[Selector]): List[Event] =
    val loggers = Array(NoLogger: Logger)
    val events = ArrayBuffer.empty[Event]
    val handler: EventHandler = (e: Event) => events.append(e)
    val framework = new Specs2Framework()
    val runner = framework.runner(Array.empty, Array.empty, getClass.getClassLoader)
    val fqcn = classOf[HelperSpec].getName

    val taskDef = new TaskDef(fqcn, Fingerprints.fp1m, true, selectors.toArray)
    val tasks = runner.tasks(Array(taskDef))
    tasks.foreach(_.execute(handler, loggers))

    events.toList

  private def testName(event: Event): String =
    event.selector() match
      case ts: TestSelector => ts.testName()

private class HelperSpec extends Specification:
  def is = s2"""
 The helper spec
   has a successful test $ok
   has a failing test $ko
   .* $ok
"""

private object NoLogger extends Logger:
  override def ansiCodesSupported(): Boolean = false
  override def error(msg: String): Unit = ()
  override def warn(msg: String): Unit = ()
  override def info(msg: String): Unit = ()
  override def debug(msg: String): Unit = ()
  override def trace(t: Throwable): Unit = ()
