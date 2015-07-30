package org.specs2
package data

import scala.collection.mutable.ListBuffer

/**
 * Topological sorting is used to define the order of execution of dependent specifications
 * when they form an acyclic graph
 */
object TopologicalSort {
  /**
   * sort elements topologically so that element at position i doesn't depend on element at position j if i < j
   * dependsOn(e1, e2) returns true if e1 depends on e2
   * returns None if there is a cycle
   *
   * Here's the algorithm from Wikipedia
   *
   * L ← Empty list that will contain the sorted nodes
   *  while there are unmarked nodes do
   *      select an unmarked node n
   *      visit(n)
   *  function visit(node n)
   *      if n has a temporary mark then stop (not a DAG)
   *      if n is not marked (i.e. has not been visited yet) then
   *          mark n temporarily
   *          for each node m with an edge from n to m do
   *              visit(m)
   *          mark n permanently
   *          add n to head of L
   */
  def sort[T](elements: Seq[T], dependsOn: (T, T) => Boolean): Option[Vector[T]] = {
    // simple node structure to tag if a node has been visited or not
    class Node(val t: T, var permanent: Boolean = false, var temp: Boolean = false) {
      override def toString = t.toString+"-"+(if (unmarked) "u" else if (temp) "t" else "p")
      def unmarked     = !permanent && !temp

      def setTemp()      = { temp = true }
      def setPermanent() = { permanent = true; temp = false }
    }
    class CycleException extends Exception

    val processed = elements.map(t => new Node(t))
    val result    = new ListBuffer[T]

    def visit(n: Node) {
      if (n.temp) throw new CycleException
      else if (!n.permanent) {
        n.setTemp
        processed.filter(m => dependsOn(n.t, m.t)).foreach(visit)
        n.setPermanent
        result.prepend(n.t)
      }
    }

    def run: Option[Vector[T]] = {
      try {
        processed.find(_.unmarked) match {
          case Some(e) => visit(e); run
          case None    => Some(result.toVector)
        }
      } catch { case e: CycleException => None }
    }
    run
  }
}
