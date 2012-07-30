package org.specs2
package collection

import Seqx._
import matcher.DataTables

class SeqxSpec extends mutable.Specification with ScalaCheck with DataTables {

  "updateLast modifies the last element".txt;
    eg { Seq(1).updateLast(i => i+1) must_== Seq(2) };
    eg { Seq(1, 2).updateLast(i => i+1) must_== Seq(1, 3) }
    eg { Seq[Int]().updateLast(i => i+1) must_== Seq[Int]() };endp

  "delta removes elements, leaving duplicates, and using a custom comparison function".txt
    "for example, comparing only the second element of a pair" >> {
      val compare = ((p: (Int, Symbol), o: Symbol) => p._2 == o)
      Seq((1, 'a), (2, 'b), (3, 'c), (4, 'b), (5, 'e)).delta(Seq('c, 'b, 'a), compare) must_==  Seq((4, 'b), (5, 'e))
    }; endp

  "A removeFirst function" should {
    "remove the first element satisfying a predicate" in {

      "Seq"          | "Element to remove" | "Result"        |>
      (Nil:Seq[Int]) ! 2                   ! (Nil:Seq[Int])  |
       Seq(2, 3, 4)  ! 2                   ! Seq(3, 4)       |
       Seq(1, 2, 2)  ! 2                   ! Seq(1, 2)       |
       Seq(1, 2, 3)  ! 2                   ! Seq(1, 3)       | { (l, a, r) =>
        l.removeFirst(_ == a) must_== r
      }
    }
    "this should work for any Seq and any element" in prop { (l: List[Int], a: Int) =>
      val removed = l removeFirst (_ == a)

      val (withoutA, startWithA) = l span (_ != a)
      removed must_== withoutA ++ startWithA.drop(1)
    }
  }

}