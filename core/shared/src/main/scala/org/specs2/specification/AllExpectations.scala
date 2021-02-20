package org.specs2
package specification

import execute.*
import main.*
import matcher.*
import core.*
import create.*
import scala.reflect.Selectable.reflectiveSelectable

/**
 * This trait can be mixed-in a specification to allow examples to have all of their expectations being evaluated (unless
 * the example body throws an exception of course).
 *
 * All the results are collected into a list, provided by the StoredExpectations trait. These results form then the body
 * of the each example (decorated by a special ExampleFactory) so that each example returns a Result which is the summary
 * of all the individual issues.
 *
 * It must be noted that this trait relies on a mutable list to collect the results as they are created in the example body.
 * Because of this restriction, a Specification using that trait must run sequentially
 *
 * If the specification is not sequential we force it to be
 */
trait AllExpectations extends SpecificationStructure with StoredExpectations with ArgumentsCreation with StandardResults:

  /**
   * we force the specification to be sequential if it's not already
   * this is important because when an example runs, its results are being stored into a shared list
   */
  /** modify the specification structure */
  override def map(structure: SpecStructure): SpecStructure =
    super.map(structure.setArguments(structure.arguments <| args(sequential = ArgProperty(true))))

  override def flatMap(f: Fragment): Fragments =
    super.flatMap(f.updateResult { r =>
      // evaluate r, triggering side effects
      val asResult = AsResult(r)

      val results = storedResults
      // if the execution catches an issue which was not stored as a side-effect
      // this is the case with a must be_==(b).orThrow for example
      // then we need to add this failure as well to the list
      if asResult.isIssue && !results.contains(asResult)then
        Result.issues(results :+ asResult, "\n")
      else
        Result.issues(results, "\n")
    })

/**
 * This trait evaluates expectations and stores them in a local variable for further usage
 */
trait StoredExpectations extends Expectations with StandardResults:
  private[specs2] lazy val results = new scala.collection.mutable.ListBuffer[Result]

  def storedResults: scala.collection.Seq[Result] =
    val failures = results.filterNot(_.isSuccess)

    // if there are several failures, indicate the location of each one
    val rs: Seq[Result] = results.toSeq.map {
      case f: Failure if failures.size > 1 =>
        f.copy(m = addLocation(f.message, f.location))
      case other =>
        other
    }
    results.clear()
    rs

  private def addLocation(message: String, location: String): String =
    val locationMessage = s" [$location]"
    message + (if !message.endsWith(locationMessage) then locationMessage else "")

  /** use a side-effect to register a standard result */
  override def skipped(message: String): Skipped =
    val r = Skipped(message)
    checkResultFailure(r)
    r

  /** use a side-effect to register a standard result */
  override def anError: Error =
    val r = Error("error")
    checkResultFailure(r)
    r

  /** use a side-effect to register a standard result */
  override def failure(message: String): Failure =
    val r = Failure(message)
    checkResultFailure(r)
    r

  override protected def checkResultFailure(r: =>Result): Result =
    results.append(r)
    r

  override def sandboxResult(r: =>Result): Result = synchronized {
    val resultsCopy = new scala.collection.mutable.ListBuffer[Result]
    resultsCopy ++= results
    try r
    finally
      results.clear
      results ++= resultsCopy
      ()
  }
