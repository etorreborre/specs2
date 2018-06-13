package org.specs2
package matcher

import execute._

/**
 * This trait evaluates expectations and stores them in a local variable for further usage
 */
trait StoredExpectations extends Expectations {
  private[specs2] lazy val results = new scala.collection.mutable.ListBuffer[MatchResult[_]]

  override protected def checkMatchResultFailure[T](m: MatchResult[T]): MatchResult[T] = {
    results.append(m)
    m
  }

  def storedResults: scala.collection.Seq[Result] = {
    val failures = results.filterNot(_.isSuccess)
    val rs = results.map {
      case f: MatchFailure[_] if failures.size > 1 =>
        f.copy(ok = () => addLocation(f.okMessage, f.toResult.location), ko = () => addLocation(f.koMessage, f.toResult.location))

      case other => other
    }.map(_.toResult)
    results.clear()
    rs
  }

  def addLocation(message: String, location: String): String = {
    val locationMessage = s" [$location]"
    message + (if (!message.endsWith(locationMessage)) locationMessage else "")
  }

  override def sandboxMatchResult[T](mr: =>MatchResult[T]): MatchResult[T] = synchronized {
    val resultsCopy = new scala.collection.mutable.ListBuffer[MatchResult[_]]
    resultsCopy.appendAll(results)
    try mr
    finally {
      results.clear
      results.appendAll(resultsCopy)
    }
  }
}

