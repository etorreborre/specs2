package org.specs2
package scalacheck

import language.adhocExtensions
import org.scalacheck.util.Pretty.*
import org.scalacheck.util.{FreqMap, Pretty}
import org.scalacheck.{Gen, Prop, Properties, Test}
import execute.*
import matcher.*
import PrettyDetails.*
import org.scalacheck.rng.Seed

import scala.util.control.NonFatal

trait ScalaCheckPropertyCheck extends ExpectationsCreation:

  def checkProperties(
      properties: Properties,
      parameters: Parameters,
      prettyFreqMap: FreqMap[Set[Any]] => Pretty
  ): Result =
    val prop = Prop { (params: Gen.Parameters) =>
      Prop.all(properties.properties.toList.map { case (n, p) => p :| n }*)(params)
    }
    check(prop, parameters, prettyFreqMap)

  /** checks if the property is true for each generated value, and with the specified parameters
    */
  def check(prop: Prop, parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty): Result =
    /** SEED CAPTURE We capture the first seed used to run the property in order to be able to display it. This seed is
      * either the first random seed used by ScalaCheck, which we capture with a mutable variable, or the seed set by
      * the user
      */
    var capturedSeed: Seed = null

    lazy val initialSeed = Option(capturedSeed)
      .orElse(parameters.seed)
      .getOrElse(
        throw new Exception(
          "A seed could not be captured for a ScalaCheck property and no seed was set on the prop or " +
            "set on the command line. Please report this issue to http://github.com/etorreborre/specs2/issues"
        )
      )

    // make a new property which will capture the first seed used by the property
    def propWithCapturedSeed = Prop { prms0 =>
      val (prms, seed) = prms0.initialSeed match
        case Some(sd) => (prms0, sd)
        case _ =>
          val sd = Seed.random()
          (prms0.withInitialSeed(sd), sd)
      capturedSeed = Option(capturedSeed).getOrElse(seed)
      prop(prms)
    }

    val result = Test.check(parameters.testParameters, propWithCapturedSeed)

    val prettyTestResult = prettyResult(result, parameters, initialSeed, prettyFreqMap)(parameters.prettyParams)
    val testResult = if parameters.prettyParams.verbosity == 0 then "" else prettyTestResult

    val checkResult =
      result match
        case Test.Result(Test.Passed, succeeded, discarded, fq, _) =>
          Success(prettyTestResult, testResult, succeeded)

        case Test.Result(Test.Proved(as), succeeded, discarded, fq, _) =>
          Success(prettyTestResult, testResult, succeeded)

        case Test.Result(Test.Exhausted, n, _, fq, _) =>
          Failure(prettyTestResult)

        case Test.Result(Test.Failed(args, labels), n, _, fq, _) =>
          new Failure(prettyTestResult, details = collectDetails(fq)) {
            // the location is already included in the failure message
            override def location = ""
          }

        case Test.Result(Test.PropException(args, ex, labels), n, _, fq, _) =>
          ex match
            case FailureException(f) =>
              // in that case we want to represent a normal failure
              val failedResult =
                prettyResult(result.copy(status = Test.Failed(args, labels)), parameters, initialSeed, prettyFreqMap)(
                  parameters.prettyParams
                )
              Failure(failedResult + "\n> " + f.message, details = f.details, trace = f.trace)

            case DecoratedResultException(DecoratedResult(_, f)) =>
              // in that case we want to represent a normal failure
              val failedResult =
                prettyResult(result.copy(status = Test.Failed(args, labels)), parameters, initialSeed, prettyFreqMap)(
                  parameters.prettyParams
                )
              f.setMessage(failedResult + "\n>\n" + f.message)

            case e: AssertionError =>
              val failedResult =
                prettyResult(result.copy(status = Test.Failed(args, labels)), parameters, initialSeed, prettyFreqMap)(
                  parameters.prettyParams
                )
              Failure(failedResult + "\n> " + e.getMessage, trace = e.getStackTrace.toList)

            case SkipException(s)    => s
            case PendingException(p) => p
            case NonFatal(t)         => Error(prettyTestResult + showCause(t), t)

    checkResultFailure(checkResult)

  /** @return the cause of the exception as a String if there is one */
  def showCause(t: Throwable) =
    Option(t.getCause).map(s"\n> caused by " + _).getOrElse("")

  def frequencies(fq: FreqMap[Set[Any]], parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty) =
    val noCollectedValues = parameters.prettyParams.verbosity <= 0 || fq.getRatios.map(_._1).forall(_.toSet == Set(()))
    if noCollectedValues then ""
    else "\n" ++ prettyFreqMap(removeDetails(fq))(parameters.prettyParams)

  /** copied from ScalaCheck to be able to inject the proper freqMap pretty */
  def prettyResult(
      res: Test.Result,
      parameters: Parameters,
      initialSeed: =>Seed,
      freqMapPretty: FreqMap[Set[Any]] => Pretty
  ) = Pretty { prms =>

    def displaySeed: String =
      if prms.verbosity >= 0 then s"\nThe seed is ${initialSeed.toBase64}\n"
      else ""

    def labels(ls: scala.collection.immutable.Set[String]) =
      if ls.isEmpty then ""
      else s"> Labels of failing property:${ls.mkString("\n")}"

    val s = res.status match
      case Test.Proved(args) =>
        s"OK, proved property.${prettyArgs(args)(prms)}" +
          (if prms.verbosity > 1 then displaySeed else "")

      case Test.Passed =>
        "OK, passed " + res.succeeded + " tests." +
          (if prms.verbosity > 1 then displaySeed else "")

      case Test.Failed(args, l) =>
        s"Falsified after " + res.succeeded + s" passed tests.\n${labels(l)}${prettyArgs(args)(prms)}" +
          displaySeed

      case Test.Exhausted =>
        "Gave up after only " + res.succeeded + " passed tests. " + res.discarded + " tests were discarded." +
          displaySeed

      case Test.PropException(args, e, l) =>
        s"Exception raised on property evaluation.${labels(l)}${prettyArgs(args)(prms)}> Exception: " + pretty(
          e,
          prms
        ) +
          displaySeed
    val t = if prms.verbosity <= 1 then "" else "Elapsed time: " + prettyTime(res.time)
    val map = freqMapPretty(res.freqMap).apply(prms)
    s"$s$t$map"
  }
