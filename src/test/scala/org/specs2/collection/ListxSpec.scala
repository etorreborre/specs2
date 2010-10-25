package org.specs2
package collection
import Listx._

class ListxSpec extends SpecificationWithJUnit { def is = 
  "A removeFirst function should" ^
  "  remove nothing if the list is empty" ! { 
      (Nil: List[String]).removeFirst(_ == "a") must_== Nil
  }^
  "  remove only the first element of a list satisfying the predicate" ! {
      List("a", "b", "c", "b").removeFirst(_ == "b") must_== List("a", "c", "b")
  }
}