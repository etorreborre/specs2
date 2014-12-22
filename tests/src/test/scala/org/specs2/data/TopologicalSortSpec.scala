package org.specs2
package data

import data.TopologicalSort._
import matcher._

class TopologicalSortSpec extends Spec with ScalaCheck{ def is = s2"""

  elements are sorted topologically
    simple example                                   $a1
    wikipedia example                                $a2
    i doesn't depend on j if i < j                   $a3
    simple cycle                                     $a4
                                                     """

  def a1 = sort[Int](Seq(3, 2, 1), (i, j) => i < j) must_== Some(Seq(1, 2, 3))

  def a2 = {
    val edges = (i: Int, j: Int) => List((7, 11), (7, 8), (5, 11), (3, 8), (11, 2), (11, 9), (8, 9), (3, 10)).contains((i, j))
    sort[Int](Seq(2, 3, 5, 7, 8, 9, 10, 11), edges) must_== Some(Vector(7, 5, 11, 3, 10, 8, 9, 2))
  }

  def a3 = prop { map: Map[Int, Int] =>
    val edges = map.filterNot { case (k, j) => k == j }
    val list = (edges.keys ++ edges.values).toSeq.distinct

    val sorted = sort[Int](list, (i: Int, j: Int) => edges.toSeq.contains((i, j)))
    (0 to sorted.size - 1).flatMap(i => (i to sorted.size - 1).map((i, _))) must not (contain((p: (Int, Int)) => edges.toSeq.contains(p)))
  }

  def a4 = sort[Int](Seq(1, 2), (i: Int, j: Int) => true) must beNone
}
