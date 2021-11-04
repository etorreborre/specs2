package org.specs2
package specification

import main.Arguments
import reporter.*
import reporter.PrinterLogger.*
import specification.core.*
import control.*
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.*, duration.*
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.function.UnaryOperator

class ResourceSpec(using ec: ExecutionContext) extends Specification:
  def is = s2"""

 A resource can be:

  - acquired for the duration of a specification
  - used by each example
  - released at the end

That resource is guaranteed to be released even if there are errors or exceptions during
the execution of the specification

  acquire and release $demo
  if the resource cannot be acquired the examples are skipped $acquireError

A global resource can be acquired across several specifications by overriding the `resourceKey`
method. It can then be accessed concurrently by several specifications
  global $concurrentGlobal

"""

  def demo =
    val messages = new ArrayBuffer[String]
    runSpec(ResourceExample(messages))
    messages.toList === List("acquired", "e1 0", "e2 1", "released with value 2")

  def acquireError =
    val logger = stringPrinterLogger
    val env = Env(printerLogger = logger)
    runSpec(AcquireErrorExample(), printer = Some(TextPrinter(env)))
    logger.messages must contain(allOf(=~("resource unavailable"), =~("o e1"), =~("o e2")))

  def runSpec(s: SpecificationStructure, printer: Option[Printer] = None) =
    val env: Env = Env(arguments = Arguments(), printerLogger = NoPrinterLogger)
    try Reporter.create(printer.toList, env).report(s.structure).runVoid(env.executionEnv)
    finally env.awaitShutdown()

  def concurrentGlobal =
    val env: Env = Env(arguments = Arguments(), printerLogger = NoPrinterLogger)
    val reporter = Reporter.create(List(), env)
    val messages: AtomicReference[List[String]] = AtomicReference(List())
    val specifications = (1 to 5).map(n => GlobalResourceExample(n, messages).structure)
    try Await.result(
      Future.sequence(specifications.map { s =>
        reporter.report(s).runFuture(env.executionEnv)
      }),
      Duration.Inf
    )
    finally env.awaitShutdown()

    (messages.get.headOption === Some("acquired")) and
      (messages.get.lastOption === Some("released with value 5")) and
      (messages.get.toSet ===
        Set(
          "acquired",
          "ref is 1",
          "ref is 2",
          "ref is 3",
          "ref is 4",
          "ref is 5",
          "released with value 5"
        ))

/** HELPERS */

class ResourceExample(messages: ArrayBuffer[String]) extends Specification, Resource[Ref[Int]]:
  def is = sequential ^ s2"""
    e1 $e1
    e2 $e2
    """

  def e1 = { (ref: Ref[Int]) =>
    messages.append("e1 " + ref.get)
    ref.update(v => v + 1)
    // the resource will be released even if there is an exception here
    throw Exception("boom")
    ok
  }

  def e2 = { (ref: Ref[Int]) =>
    messages.append("e2 " + ref.get)
    ref.update(v => v + 1)
    ok
  }

  def acquire =
    messages.append("acquired")
    Future.successful(Ref(0))

  def release(ref: Ref[Int]) = Execution.result {
    messages.append("released with value " + ref.get)
    ok
  }

class AcquireErrorExample extends Specification, Resource[Ref[Int]]:
  def is = sequential ^
    "e1" ! e1 ^
    "e2" ! e2

  def e1: Execution = { (ref: Ref[Int]) => ok }
  def e2: Execution = { (ref: Ref[Int]) => ok }

  def acquire =
    Future.failed(Exception("boom"))

  def release(ref: Ref[Int]) =
    ok

trait GlobalResource extends Resource[AtomicInteger]:

  override def resourceKey: Option[String] =
    Some("global")

  val messages: AtomicReference[List[String]]
  def append(m: String) =
    messages.getAndUpdate(new UnaryOperator[List[String]] {
      def apply(l: List[String]): List[String] = l :+ m
    })

  def acquire =
    append("acquired")
    Future.successful(new AtomicInteger(0))

  def release(ref: AtomicInteger) = Execution.result {
    append("released with value " + ref.get)
    true
  }

case class GlobalResourceExample(number: Int, messages: AtomicReference[List[String]])
    extends Specification,
      GlobalResource:
  def is = s2"""e1 $e1"""

  def e1 = { (ref: AtomicInteger) =>
    append("ref is " + ref.incrementAndGet.toString)
    ok
  }
