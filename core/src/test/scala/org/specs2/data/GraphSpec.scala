package org.specs2
package data

import Graph._

class GraphSpec extends Specification { def is = s2"""

 The elements accessible transitively from a given element must be returned $transitive
 Even if there is a cycle $cycle

"""

  def transitive = {
    val map = Map(1 -> List(2), 2 -> List(3, 4))
    transitiveClosure[Int, Int](1, (t: Int) => map.getOrElse(t, Nil), identity _).toSet must_== Set(1, 2, 3, 4)
  }

  def cycle = {
    val map = Map(1 -> List(2), 2 -> List(3, 4), 3 -> List(1))
    transitiveClosure[Int, Int](1, (t: Int) => map.getOrElse(t, Nil), identity _).toSet must_== Set(1, 2, 3, 4)
  }
}
