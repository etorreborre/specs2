package org.specs2
package matcher

import analysis._
import execute.Result
import control._

/**
 * The dependency matchers trait provides a way to specify the dependencies that should be verified in your project
 * and then to check that there's no unwanted dependency in the code.
 *
 * It does so by:
 *
 *  - specifying the dependencies as "layers" where a package name declared in one layer can only be dependent on a package name
 *    declared in a lower layer
 *
 *  - using the scala compiler BuildManager class to recompile the files and get dependency analysis data
 *
 * Usage:
 *
 *   layers(
 *     "package1",
 *     layer("package2", "package3", "package4"),
 *     layer("package6", "package7")
 *   ) must beRespected
 *
 */
trait DependencyMatchers extends DependencyBaseMatchers with DependencyBeHaveMatchers

trait DependencyBaseMatchers extends LayersAnalysis {

  /** matcher for Layers declarations */
  def beRespected = new LayersDependenciesMatcher

  /**
   * this matcher checks that dependencies are satisfied for a given Layers definition
   */
  class LayersDependenciesMatcher extends Matcher[Layers] {
    def apply[S <: Layers](ls: Expectable[S]) = {
      ls.value.unsatisfied.execute(noLogging).unsafePerformIO.fold(
        unsatisfied => result(unsatisfied.isEmpty, "all dependencies are satisfied", "those dependencies are not satisfied:\n"+unsatisfied.showBreaks, ls),
        issue       => result(Result.theseToResult(issue), ls)
      )

    }
  }

}

trait DependencyBeHaveMatchers extends BeHaveMatchers { outer: DependencyBaseMatchers =>

  implicit def toLayersResultMatcher(result: MatchResult[Layers]) = new LayersResultMatcher(result)
  class LayersResultMatcher(result: MatchResult[Layers]) {
    def beRespected = result(outer.beRespected)
  }

  def respected = beRespected

}