package org.specs2
package matcher

import execute._

/**
 * This trait evaluates expectations and stores them in a local variable for further usage
 */
trait StoredExpectations extends Expectations {
  private[specs2] lazy val results = new scala.collection.mutable.ListBuffer[Result]

  override protected def checkResultFailure(r: =>Result): Result = {
    results.append(AsResult(r))
    Success()
  }

  override protected def checkMatchResultFailure[T](m: MatchResult[T]): MatchResult[T] = {
    checkResultFailure(AsResult(m))
    m
  }

  def storedResults: Seq[Result] = {
    val rs = results.toSeq
    results.clear()
    rs
  }
}

