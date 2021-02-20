package org.specs2
package specification

import main.Arguments
import reporter.*
import reporter.PrinterLogger.*
import specification.core.*
import control.*
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.*

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

"""

  def demo =
    val messages = new ArrayBuffer[String]
    runSpec(ResourceExample(messages))
    messages.toList ==== List("acquired", "e1 0", "e2 1", "released with value 2")

  def acquireError =
    val logger = stringPrinterLogger
    val env = Env(printerLogger = logger)
    runSpec(AcquireErrorExample(), printer = Some(TextPrinter(env)))
    logger.messages `must` contain(allOf(=~("resource unavailable"), =~("o e1"), =~("o e2")))

  def runSpec(s: SpecificationStructure, printer: Option[Printer] = None) =
    val env: Env = Env(arguments = Arguments(), printerLogger = NoPrinterLogger)
    try Reporter.create(printer.toList, env).report(s.structure).runVoid(env.executionEnv)
    finally env.shutdown()

class ResourceExample(messages: ArrayBuffer[String]) extends Specification with Resource[Ref[Int]]:
  def is = sequential ^ s2"""
    e1 $e1
    e2 $e2
    """

  def e1 = { (ref: Ref[Int]) =>
    messages.append("e1 "+ref.get)
    ref.update(v => v + 1)
    // the resource will be released even if there is an exception here
    throw Exception("boom")
    ok
  }

  def e2 = { (ref: Ref[Int]) =>
    messages.append("e2 "+ref.get)
    ref.update(v => v + 1)
    ok
  }

  def acquire =
    messages.append("acquired")
    Future.successful(Ref(0))

  def release(ref: Ref[Int]) =
    messages.append("released with value "+ref.get)
    ok

class AcquireErrorExample extends Specification with Resource[Ref[Int]]:
  def is = sequential ^ s2"""
    e1 $e1
    e2 $e2
    """

  def e1 = { (ref: Ref[Int]) => ok }
  def e2 = { (ref: Ref[Int]) => ok }

  def acquire =
    Future.failed(Exception("boom"))

  def release(ref: Ref[Int]) =
    ok
