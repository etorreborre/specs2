package org.specs2
package scalacheck

import org.scalacheck.Prop.Arg
import org.scalacheck.util.Pretty._
import org.scalacheck.util.{FreqMap, Pretty}
import org.scalacheck.{Gen, Properties, Prop, Test}
import execute._
import matcher._
import text.NotNullStrings._

trait ScalaCheckPropertyCheck {

  /**
   * checks if the property is true for each generated value, and with the specified
   * scalacheck parameters. If verbose is true, then print the results on the console
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
  }

  def counterExampleMessage(args: List[Prop.Arg[Any]], n: Int, labels: Set[String]) =
    "A counter-example is "+counterExample(args)+" (" + afterNTries(n) + afterNShrinks(args) + ")" + failedLabels(labels)

  // depending on the result, return the appropriate success status and messages
  // the failure message indicates a counter-example to the property
  def noCounterExample(n: Int) = "The property passed without any counter-example " + afterNTries(n)

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


  def afterNTries(n: Int) = "after " + (if (n <= 1) n + " try" else n + " tries")

  def afterNShrinks(args: List[Arg[_]]) = {
    if (args.forall(_.shrinks == 0))  ""
    else
      args.map { arg =>
        if (arg.origArg != arg.arg) "'"+arg.origArg +"' -> '"+arg.arg+"'"
        else " = "
      }.mkString(" - shrink (", ",", ")")
  }

  /** @return the cause of the exception as a String if there is one */
  def showCause(e: java.lang.Exception) =
    Option(e.getCause).map(s"\n> caused by "+_).getOrElse("")

  def counterExample(args: List[Arg[_]]) = {
    if (args.size == 1)
      args.map(a => a.arg.notNull).mkString("'", "", "'")
    else if (args.exists(_.arg.notNull.isEmpty))
      args.map(_.arg.notNull).mkString("['", "', '", "']")
    else
      args.map(_.arg.notNull).mkString("[", ", ", "]")
  }
  def failedLabels(labels: Set[String]) = {
    if (labels.isEmpty)  ""
    else labels.mkString("\n", ", ", "\n")
  }

  def frequencies(fq: FreqMap[Set[Any]], parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty) = {
    val noCollectedValues = parameters.prettyParams.verbosity <= 0 || fq.getRatios.map(_._1).forall(_.toSet == Set(()))

    if (noCollectedValues) ""
    else "\n" ++ prettyFreqMap(fq)(parameters.prettyParams)
  }

}
