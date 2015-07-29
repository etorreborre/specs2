package org.specs2
package matcher

import data.HopcroftKarp._
import execute.BestMatching
import BestMatching._
import MatchResult._

class HopcroftKarpSpec extends Spec { def is = s2"""

 The Hopcroft-Karp algorithm can be used to efficiently find the maximum
 matching of a bipartite graph

  find the maximal matching in a bipartite graph         $g1
  another example                                        $g2

  find the best association between values and matchers
    and return the least number of failures              $g3


  traversable test $t1

"""

  def t1 = {
    Seq(1, 2, 3) must contain(atLeast(be_>=(0), be_>=(1), be_<=(1)))
  }

  def g1 = {
    val graph = Map(1 -> List(6, 7, 8),
                    2 -> List(),
                    3 -> List(6),
                    4 -> List(8),
                    5 -> List(6, 9))

    findMaximalMatching(1 to 5, 6 to 9, graph) must contain((1, 7), (3, 6), (4, 8), (5, 9))
  }

  def g2 = {
    val graph = Map(
      1 -> List(6, 8, 10),
      2 -> List(6),
      3 -> List(6),
      4 -> List(9),
      5 -> List(10))

    findMaximalMatching(1 to 5, 6 to 10, graph) must contain((1, 8), (2, 6), (4, 9), (5, 10))
  }

  def g3 = {
    // the best match leaves 3 out
    // 1 matches with List(1)
    // 2 matches with List(1, 2, 3)
    // 4 matches with List(4)
    // 5 matches with List(1, 5)
    val (matches, remaining) =
    findBestMatch[Int, List[Int], MatchResult[_]](
      1 to 5, List(
                   List(1, 2, 3),
                   List[Int](),
                   List(1),
                   List(4),
                   List(1, 5)),
                  (i: Int, list: List[Int]) => if (list.contains(i)) ok(s"$list contains $i") else ko(s"$list does not contain $i"))

    matches.map { case (i, j, r) => r.message } must contain(exactly(
      "List(1) contains 1",
      "List(1, 2, 3) contains 2",
      "List(4) contains 4",
      "List(1, 5) contains 5",
      "List() does not contain 3"))
  }

}

