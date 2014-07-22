package org.specs2
package matcher

import org.specs2.execute.{Result, AsResult}

import data.HopcroftKarp._

/**
 * Use of the Hopcroft-Karp (https://en.wikipedia.org/wiki/Hopcroft%E2%80%93Karp_algorithm) algorithm for
 * specs2, to the minimum set of mismatched elements, and corresponding failures
 */
object BestMatching {

  /**
   * This function indexes values of type T and V, and the result of their matching
   * so that the algorithm can work on a simplified graph form of just of matching elements represented by integers
   */
  def findBestMatch[T, V, R : AsResult](ts: Seq[T], vs: Seq[V], matchingFunction: (T, V) => R): Seq[(T, V, Result)] = {
    // perform n^2 matches
    val matches: List[(T, Int, V, Int, Result)] =
      ts.zipWithIndex.foldLeft(List[(T, Int, V, Int, Result)]()) { case (res, (t, i)) =>
        val results = vs.zipWithIndex.map { case (v, j) =>
          (t, i, v, ts.size + j, AsResult(matchingFunction(t, v)))
        }
        res ++ results
      }
    // edges of the graph in the form of a map from t vertex in the first part of the bipartite graph to a list of vertices
    // in the second part
    val startMap = Map[Int, Seq[Int]]().withDefault(_ => Vector())
    val edges = matches.foldLeft(startMap) { case (res, (t, i, v, j, r)) =>
      if (r.isSuccess) res + (i -> (res(i) :+ j)) else res
    }

    // find the maximal matching
    val best = findMaximalMatching(0 until ts.size, (0 until vs.size).map(_ + ts.size), edges)

    // collect one failure per unmatched value
    val keys = best.toMap.keys.toVector
    matches.collect { case (t, i, v, j, r) if !keys.contains(i) && !r.isSuccess =>
      (i, (t, v, r))
    }.groupBy(_._1).mapValues(_.head._2).values.toList
  }

}

