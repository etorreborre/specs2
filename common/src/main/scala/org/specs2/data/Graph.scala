package org.specs2
package data

object Graph {

  /** compute the transitive closure of an element given a relation and a way to identify transitive elements */
  def transitiveClosure[T, S](t: T, related: T => List[T], identify: T => S): List[T] = {
    def getAll(seed: List[T], visited: Map[S, T]): List[T] = {
      if (seed.isEmpty) visited.values.toList
      else {
        val toVisit: Map[S, T] = (seed ++ seed.flatMap(related)).groupBy(identify).mapValues(_.head).filterNot { case (n, _) => visited.keys.toSeq.contains(n) }
        getAll(toVisit.values.toList, visited ++ toVisit)
      }
    }
    val id = identify(t)
    val linked = related(t).filter(u => identify(u) != identify(t)).map(u => (identify(u), u))
    getAll(List(t), Map())
  }

}
