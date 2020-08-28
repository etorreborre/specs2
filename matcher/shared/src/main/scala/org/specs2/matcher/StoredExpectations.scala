package org.specs2
package matcher

import execute._

/**
 * This trait evaluates expectations and stores them in a local variable for further usage
 */
trait StoredExpectations extends Expectations:
  private[specs2] lazy val matchResults = new scala.collection.mutable.ListBuffer[MatchResult[_]]
  private[specs2] lazy val results = new scala.collection.mutable.ListBuffer[Result]

  def storedResults: scala.collection.Seq[Result] =
    val failures = matchResults.filterNot(_.isSuccess)

    // if there are several failures, indicate the location of each one
    val rs: Seq[Result] = matchResults.toSeq.map {
      case f: MatchFailure[_] if failures.size > 1 =>
        f.copy(
          ok = () => addLocation(f.okMessage, f.toFailure.location),
          ko = () => addLocation(f.koMessage, f.toFailure.location))

      case other => other
    }.map(_.toResult) ++ results.toSeq
    matchResults.clear()
    results.clear()
    rs

  def addLocation(message: String, location: String): String =
    val locationMessage = s" [$location]"
    message + (if !message.endsWith(locationMessage) then locationMessage else "")

  override protected def checkMatchResultFailure[T](m: MatchResult[T]): MatchResult[T] =
    matchResults.append(m)
    m

  override protected def checkResultFailure(r: =>Result): Result =
    results.append(r)
    r

  override def sandboxMatchResult[T](mr: =>MatchResult[T]): MatchResult[T] = synchronized {
    val matchResultsCopy = new scala.collection.mutable.ListBuffer[MatchResult[_]]
    matchResultsCopy ++= matchResults
    try mr
    finally
      matchResults.clear
      matchResults ++= matchResultsCopy
      ()
  }

