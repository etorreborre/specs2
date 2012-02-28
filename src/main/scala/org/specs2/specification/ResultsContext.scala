package org.specs2
package specification

import execute.Result
import execute.Result._
import matcher.StoredExpectations

/**
 * This class is used to evaluate a Context as a sequence of results by side-effects.
 *
 * @see the AllExpectations trait for its use
 */
class ResultsContext(results: =>Seq[Result]) extends StoredResultsContext {
  def storedResults = results
}

/**
 * This trait can be used when it is not desirable to use the AllExpectations trait, that is, when the specification
 * examples must be executed concurrently and not isolated.
 *
 * @see the UserGuide on how to use this trait
 */
trait StoredExpectationsContext extends StoredExpectations with StoredResultsContext

/**
 * This trait is a context which will use the results provided by the class inheriting that trait.
 * It evaluates the result of an example, which is supposed to create side-effects
 * and returns the 'storedResults' as the summary of all results
 */
trait StoredResultsContext extends Context { this: { def storedResults: Seq[Result]} =>
  def apply[T <% Result](r: =>T): Result = {
    // evaluate r, triggering side effects
    r
    issues(storedResults, "\n")
  }
}
