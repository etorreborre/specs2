package org.specs2
package data

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.*

/** Hopcroft-Karp (https://en.wikipedia.org/wiki/Hopcroft%E2%80%93Karp_algorithm) algorithm for finding the maximum
  * matching in a bipartite graph
  *
  * This is used for "contain" matchers to find the best matches between actual and expected elements in 2 lists
  */
object HopcroftKarp:

  def findMaximalMatching(vertex1: Seq[Int], vertex2: Seq[Int], edges: Map[Int, Seq[Int]]): List[(Int, Int)] =
    val nil = -1
    val queue: mutable.Queue[Int] = new mutable.Queue[Int]
    val dist: mutable.Map[Int, Int] = new mutable.HashMap[Int, Int]
    val pair1: mutable.Map[Int, Int] = new mutable.HashMap[Int, Int]
    val pair2: mutable.Map[Int, Int] = new mutable.HashMap[Int, Int]

    def bfs: Boolean =
      vertex1.foreach { v =>
        if pair1(v) == nil then
          dist.put(v, 0)
          queue.enqueue(v)
        else dist.put(v, Int.MaxValue)
      }
      dist.put(nil, Int.MaxValue)

      while queue.nonEmpty do
        val v = queue.dequeue

        if dist(v) < dist(nil) then
          edges.get(v).toSeq.flatten.foreach { u =>
            if dist(pair2(u)) == Int.MaxValue then
              dist.put(pair2(u), dist(v) + 1)
              queue.enqueue(pair2(u))
          }
      dist(nil) != Int.MaxValue

    def dfs(v: Int): Boolean = boundary {
      if v != -1 then
        edges.get(v).toSeq.flatten.foreach { u =>
          if dist(pair2(u)) == dist(v) + 1 && dfs(pair2(u)) then
            pair2.put(u, v)
            pair1.put(v, u)
            break(true)
        }
        dist.put(v, Int.MaxValue)
        false
      else true
    }

    ((vertex1 ++ vertex2) :+ nil).foreach { v =>
      pair1.put(v, nil)
      pair2.put(v, nil)
    }
    var matching = 0
    while bfs do
      vertex1.foreach { v =>
        if pair1(v) == nil && dfs(v) then matching = matching + 1
      }
    pair1.toList.filterNot(_._2 == nil)
