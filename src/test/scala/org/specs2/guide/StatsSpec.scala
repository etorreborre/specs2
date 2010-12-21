package org.specs2
package guide


class StatsSpec extends Specification { def is =
"Matchers guide".title ^
  "a parent spec" ^
  include(child1) ^
  include(child2) ^ end

  val child1 = new Specification { def is = "c1".title ^ "t1" ^ "e1" ! success }
  val child2 = new Specification { def is = "c2".title ^ "t2" ^ "e2" ! success }
}