package org.specs2
package specification

import core.*
import main.Arguments
import reporter.*
import reporter.PrinterLogger.*
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.*

class EnvResourcesSpec extends Specification:
  given ExecutionContext = scala.concurrent.ExecutionContext.global

  def is = sequential ^ s2"""

Specifications share resources by sharing an Env. Two Envs created independently must therefore
keep their resources to themselves, otherwise shutting one down would finalize resources that
another one is still using.

  each Env gets its own resources map $ownMap
  shutting down an Env leaves another Env's resources alone $noCrossRelease

"""

  def ownMap =
    val env1 = Env()
    val env2 = Env()
    try (env1.resources `eq` env2.resources) === false
    finally
      env1.shutdown()
      env2.shutdown()

  def noCrossRelease =
    val messages: ArrayBuffer[String] = ArrayBuffer()
    val env1 = Env(arguments = Arguments(), printerLogger = NoPrinterLogger)
    val env2 = Env(arguments = Arguments(), printerLogger = NoPrinterLogger)
    val reporter = Reporter.create(List(), env1)
    def recorded = messages.synchronized(messages.toList)
    def released = recorded.count(_.startsWith("released"))
    for
      _ <- reporter.report(GlobalResourceExample(1, messages).structure).runFuture(env1.executionEnv)
      beforeShutdown = released
      otherIssues <- env2.startShutdown
      afterOtherShutdown = released
      issues <- env1.startShutdown
    yield
      // a finalizer which fails is reported as an issue by startShutdown instead of recording a message,
      // so check the issues first: a missing "released" message alone would not tell why the release did not happen
      ((otherIssues ++ issues) must beEmpty).updateMessage("finalizing the resources failed: " + _) and
        (beforeShutdown === 0) and
        (afterOtherShutdown === 0) and
        (released === 1).updateMessage(_ + s", recorded messages: ${recorded.mkString("[", ", ", "]")}")
