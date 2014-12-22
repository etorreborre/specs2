package org.specs2
package scalacheck

import org.scalacheck.util.Pretty._
import org.scalacheck.util.{FreqMap, Pretty}
import org.scalacheck.{Gen, Properties, Prop, Test}
import execute._
import matcher._

trait ScalaCheckPropertyCheck extends ExpectationsCreation {

  /**
   * checks if the property is true for each generated value, and with the specified
   * parameters
   */
  def check(prop: Prop, parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty): Result = {
    // check the property with ScalaCheck
    // first add labels if this Prop is a composite one
    val prop1 = prop match {
      case ps: Properties =>
        new Prop {
          override def apply(params: Gen.Parameters): Prop.Result =
            Prop.all(ps.properties.map { case (n, p) => p :| n }:_*)(params)
        }
      case _ => prop
    }
    val result = Test.check(parameters.testParameters, prop1)
    val prettyTestResult = prettyTestRes(result)(parameters.prettyParams)
    val testResult = if (parameters.prettyParams.verbosity == 0) "" else prettyTestResult

    val checkResult =
      result match {
        case Test.Result(Test.Passed, succeeded, discarded, fq, _)     =>
          Success(prettyTestResult, testResult + frequencies(fq, parameters, prettyFreqMap), succeeded)

        case Test.Result(Test.Proved(as), succeeded, discarded, fq, _) =>
          Success(prettyTestResult, testResult + frequencies(fq, parameters, prettyFreqMap), succeeded)

        case Test.Result(Test.Exhausted, n, _, fq, _)              =>
          Failure(prettyTestResult + frequencies(fq, parameters, prettyFreqMap))

        case Test.Result(Test.Failed(args, labels), n, _, fq, _) =>
          new Failure(prettyTestResult + frequencies(fq, parameters, prettyFreqMap), details = collectDetails(fq)) {
            // the location is already included in the failure message
            override def location = ""
          }

        case Test.Result(Test.PropException(args, ex, labels), n, _, fq, _) =>
          ex match {
            case FailureException(f) =>
              // in that case we want to represent a normal failure
              val failedResult = prettyTestRes(result.copy(status = Test.Failed(args, labels)))(parameters.prettyParams)
              new Failure(failedResult + "\n> " + f.message + frequencies(fq, parameters, prettyFreqMap), details = f.details) { override def location = f.location }
            case SkipException(s)    => s
            case PendingException(p) => p
            case e: java.lang.Exception      =>
              Error(prettyTestResult + showCause(e) + frequencies(fq, parameters, prettyFreqMap), e)
            case throwable    => throw ex
          }
      }

    checkResultFailure(checkResult)
  }

  /** @return the cause of the exception as a String if there is one */
  def showCause(e: java.lang.Exception) =
    Option(e.getCause).map(s"\n> caused by "+_).getOrElse("")

  /**
   * Failure details might get collected with the collect operation when evaluating
   * the Prop
   */
  def collectDetails[T](fq: FreqMap[Set[T]]): execute.Details = {
    fq.getRatios.flatMap(_._1.toList).collect {
      case d : execute.FailureDetails    => d
      case d : execute.FailureSeqDetails => d
      case d : execute.FailureSetDetails => d
      case d : execute.FailureMapDetails => d
    }.headOption.getOrElse(execute.NoDetails)
  }

  /**
   * Remove failure details from collected data
   */
  def removeDetails(fq: FreqMap[Set[Any]]): FreqMap[Set[Any]] = {
    fq.getCounts.foldLeft(FreqMap.empty[Set[Any]]) { case (res, (set, count)) =>
      set.toList match {
        case (_:execute.FailureDetails) :: _    => res
        case (_:execute.FailureSeqDetails) :: _ => res
        case (_:execute.FailureSetDetails) :: _ => res
        case (_:execute.FailureMapDetails) :: _ => res
        case _                                  => (1 to count).foldLeft(res) { case (map, i) => map + set }
      }
    }
  }

  def frequencies(fq: FreqMap[Set[Any]], parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty) = {
    val noCollectedValues = parameters.prettyParams.verbosity <= 0 || fq.getRatios.map(_._1).forall(_.toSet == Set(()))

    if (noCollectedValues) ""
    else "\n" ++ prettyFreqMap(removeDetails(fq))(parameters.prettyParams)
  }

}
