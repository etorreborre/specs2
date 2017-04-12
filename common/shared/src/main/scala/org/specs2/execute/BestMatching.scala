package org.specs2.execute

import org.specs2.data.HopcroftKarp._

/**
 * Use of the Hopcroft-Karp (https://en.wikipedia.org/wiki/Hopcroft%E2%80%93Karp_algorithm) algorithm for
 * specs2, to the minimum set of mismatched elements, and corresponding failures
 */
object BestMatching {

  /**
   * This function indexes values of type T and V, and the result of their matching
   * so that the algorithm can work on a simplified graph form of just of matching elements represented by integers
   *
   * @return matched values with their result and missing values
   */
  def findBestMatch[T, V, R : AsResult](ts: Seq[T], vs: Seq[V], matchingFunction: (T, V) => R, eachCheck: Boolean = true): (Seq[(T, V, Result)], Seq[V]) = {
    // perform n^2 matches
    val matches: List[(T, Int, V, Int, Result)] =
      ts.zipWithIndex.foldLeft(List[(T, Int, V, Int, Result)]()) { case (res, (t, i)) =>
        val results = vs.zipWithIndex.map { case (v, j) =>
          (t, i, v, ts.size + j, AsResult(matchingFunction(t, v)))
        }
        res ++ results
      }
    val allResults: Map[(Int, Int), (T, V, Result)] = matches.map { case (t, i, v, j, r) => ((i, j), (t, v, r)) }.toMap

    // edges of the graph in the form of a map from t vertex in the first part of the bipartite graph to a list of vertices
    // in the second part
    val startMap = Map[Int, Seq[Int]]().withDefault(_ => Vector())
    val edges = matches.foldLeft(startMap) { case (res, (t, i, v, j, r)) =>
      if (r.isSuccess) res + (i -> (res(i) :+ j).sorted) else res
    }

    // find the maximal matching
    val best = findMaximalMatching(0 until ts.size, (0 until vs.size).map(_ + ts.size), edges)
    val successfulIndices = best.map(_._1)
    val successes =
      if (!eachCheck) allResults.collect { case ((i, j), (t, v, r)) if best.exists(_._1 == i) && r.isSuccess => (t, v, r) }.toList
      else best.sorted.map(allResults)

    // collect one failure per unmatched value
    val failures = matches.collect { case (t, i, v, j, r) if !successfulIndices.contains(i) && !best.exists(_._2 == j) && !r.isSuccess =>
      (i, (t, v, r))
    }.groupBy(_._1).mapValues(_.head._2).values.toList

    def isUnchecked(j: Int) = {
      !best.exists(_._2 == j) && (eachCheck || !allResults.exists { case ((i, j1), (_, _, r)) => j1 == j && r.isSuccess })
    }

    val unchecked = vs.zipWithIndex.collect { case (v, j) if isUnchecked(j+ts.size)  => v }

    (successes ++ failures.reverse, unchecked)
  }

}

