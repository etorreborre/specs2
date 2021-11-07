package org.specs2
package matcher

import execute._

/**
 * This trait evaluates expectations and stores them in a local variable for further usage
 */
trait StoredExpectations extends Expectations with StoredResults {
  private[specs2] lazy val matchResults = new scala.collection.mutable.ListBuffer[MatchResult[?]]
  private[specs2] lazy val results = new scala.collection.mutable.ListBuffer[Result]

  def storedResults: scala.collection.Seq[Result] = {
    val failures = matchResults.filterNot(_.isSuccess)
    val rs = matchResults.map {
      case f: MatchFailure[?] if failures.size > 1 =>
        f.copy(ok = () => addLocation(f.okMessage, f.toResult.location), ko = () => addLocation(f.koMessage, f.toResult.location))

      case other => other
    }.map(_.toResult)
    matchResults.clear

    val resultsCopy = new scala.collection.mutable.ListBuffer[Result]
    resultsCopy ++= results
    results.clear

    rs.toSeq ++ resultsCopy
  }

  def addLocation(message: String, location: String): String = {
    val locationMessage = s" [$location]"
    message + (if (!message.endsWith(locationMessage)) locationMessage else "")
  }

  override protected def checkMatchResultFailure[T](m: MatchResult[T]): MatchResult[T] = {
    matchResults.append(m)
    m
  }

  override protected def checkResultFailure(r: =>Result): Result = {
    results.append(r)
    r
  }

  override def sandboxMatchResult[T](mr: =>MatchResult[T]): MatchResult[T] = synchronized {
    val matchResultsCopy = new scala.collection.mutable.ListBuffer[MatchResult[?]]
    matchResultsCopy ++= matchResults
    try mr
    finally {
      matchResults.clear
      matchResults ++= matchResultsCopy
      ()
    }
  }

}

trait StoredResults {
  def storedResults: scala.collection.Seq[Result]
}
