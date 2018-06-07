package org.specs2
package scalacheck

import org.scalacheck.util.Pretty._
import org.scalacheck.util.{FreqMap, Pretty}
import org.scalacheck.{Gen, Prop, Properties, Test}
import execute._
import matcher._
import PrettyDetails._

import scala.util.control.NonFatal

trait ScalaCheckPropertyCheck extends ExpectationsCreation {

  def checkProperties(properties: Properties, parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty): Result = {
    val prop = Prop { params: Gen.Parameters =>
      Prop.all(properties.properties.map { case (n, p) => p :| n }:_*)(params)
    }
    check(prop, parameters, prettyFreqMap)
  }

  /**
   * checks if the property is true for each generated value, and with the specified
   * parameters
   */
  def check(prop: Prop, parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty): Result = {
    val result = Test.check(parameters.testParameters, prop)
    val prettyTestResult = prettyResult(result, prettyFreqMap)(parameters.prettyParams)
    val testResult = if (parameters.prettyParams.verbosity == 0) "" else prettyTestResult

    val checkResult =
      result match {
        case Test.Result(Test.Passed, succeeded, discarded, fq, _)     =>
          Success(prettyTestResult, testResult, succeeded)

        case Test.Result(Test.Proved(as), succeeded, discarded, fq, _) =>
          Success(prettyTestResult, testResult, succeeded)

        case Test.Result(Test.Exhausted, n, _, fq, _)              =>
          Failure(prettyTestResult)

        case Test.Result(Test.Failed(args, labels), n, _, fq, _) =>
          new Failure(prettyTestResult, details = collectDetails(fq)) {
            // the location is already included in the failure message
            override def location = ""
          }

        case Test.Result(Test.PropException(args, ex, labels), n, _, fq, _) =>
          ex match {
            case FailureException(f) =>
              // in that case we want to represent a normal failure
              val failedResult = prettyResult(result.copy(status = Test.Failed(args, labels)), prettyFreqMap)(parameters.prettyParams)
              new Failure(failedResult + "\n> " + f.message, details = f.details, stackTrace = f.stackTrace)

            case DecoratedResultException(DecoratedResult(_, f)) =>
              // in that case we want to represent a normal failure
              val failedResult = prettyResult(result.copy(status = Test.Failed(args, labels)), prettyFreqMap)(parameters.prettyParams)
              f.updateMessage(failedResult + "\n>\n" + f.message)

            case e: AssertionError =>
              val failedResult = prettyResult(result.copy(status = Test.Failed(args, labels)), prettyFreqMap)(parameters.prettyParams)
              new Failure(failedResult + "\n> " + e.getMessage, stackTrace = e.getStackTrace.toList)

            case SkipException(s)    => s
            case PendingException(p) => p
            case NonFatal(t)         => Error(prettyTestResult + showCause(t), t)
          }
      }

    checkResultFailure(checkResult)
  }

  /** @return the cause of the exception as a String if there is one */
  def showCause(t: Throwable) =
    Option(t.getCause).map(s"\n> caused by "+_).getOrElse("")

  def frequencies(fq: FreqMap[Set[Any]], parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty) = {
    val noCollectedValues = parameters.prettyParams.verbosity <= 0 || fq.getRatios.map(_._1).forall(_.toSet == Set(()))
    if (noCollectedValues) ""
    else "\n" ++ prettyFreqMap(removeDetails(fq))(parameters.prettyParams)
  }

  /** copied from ScalaCheck to be able to inject the proper freqMap pretty */
  def prettyResult(res: Test.Result, freqMapPretty: FreqMap[Set[Any]] => Pretty) = Pretty { prms =>
    def labels(ls: scala.collection.immutable.Set[String]) =
      if(ls.isEmpty) ""
      else "> Labels of failing property: " / ls.mkString("\n")
    val s = res.status match {
      case Test.Proved(args) => "OK, proved property."/prettyArgs(args)(prms)
      case Test.Passed => "OK, passed "+res.succeeded+" tests."
      case Test.Failed(args, l) =>
        "Falsified after "+res.succeeded+" passed tests."/labels(l)/prettyArgs(args)(prms)
      case Test.Exhausted =>
        "Gave up after only "+res.succeeded+" passed tests. " +
          res.discarded+" tests were discarded."
      case Test.PropException(args,e,l) =>
        "Exception raised on property evaluation."/labels(l)/prettyArgs(args)(prms)/
          "> Exception: "+pretty(e,prms)
    }
    val t = if(prms.verbosity <= 1) "" else "Elapsed time: "+prettyTime(res.time)
    val map = freqMapPretty(res.freqMap).apply(prms)
    s/t/map
  }

  private implicit class StrBreak(val s1: String) {
    def /(s2: String) = if(s2 == "") s1 else s1+"\n"+s2
  }
}
