package org.specs2
package matcher

import data.HopcroftKarp._
import BestMatching._

class HopcroftKarpSpec extends Specification { def is = s2"""

 The Hopcroft-Karp algorithm can be used to efficiently find the maximum
 matching of a bipartite graph

  find the maximal matching in a bipartite graph         $g1
  find the best association between values and matchers
    and return the least number of failures              $g2

"""

  def g1 = {
    val graph = Map(1 -> List(6, 7, 8),
                    2 -> List(),
                    3 -> List(6),
                    4 -> List(8),
                    5 -> List(6, 9))


    findMaximalMatching(1 to 5, 6 to 9, graph) must contain((1, 7), (3, 6), (4, 8), (5, 9))
  }

  def g2 = {
    // the best match leaves 3 out
    // 1 matches with List(1)
    // 2 matches with List(1, 2, 3)
    // 4 matches with List(4)
    // 5 matches with List(1, 5)
    findBestMatch(1 to 5,
                  List(List(1, 2, 3), List[Int](), List(1), List(4), List(1, 5)),
      (i: Int, list: List[Int]) => list must contain(i)).map { case (i, j, r) => r.message } must_==
      List("List() does not contain 3")
  }

}

