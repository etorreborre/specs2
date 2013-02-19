package org.specs2
package execute

/**
 * This trait can be used for anything that can be converted to a Result.
 *
 * It is used by the MatchResult class so it can be executed as any other kind of Result by the `ResultExecution` class without introducing
 * a dependency between the `execute` and the `matcher` package
 */
trait ResultLike {
  def toResult: Result
}