package org.specs2
package collection

import Seqx._
import matcher.DataTables
import org.scalacheck.Prop

class SeqxSpec extends mutable.Specification with DataTables with ScalaCheckResult {

  "updateLast modifies the last element".p
    eg { Seq(1).updateLast(i => i+1) must_== Seq(2) }
    eg { Seq(1, 2).updateLast(i => i+1) must_== Seq(1, 3) }
    eg { Seq[Int]().updateLast(i => i+1) must_== Seq[Int]() }

  "updateLastOr modifies the last element or starts a new sequence".p
    eg { Seq(1).updateLastOr { case i => i+1 }(0) must_== Seq(2) }
    eg { Seq(1, 2).updateLastOr { case i => i+1 }(0) must_== Seq(1, 3) }
    eg { Seq[Int]().updateLastOr { case i => i+1 }(0) must_== Seq[Int](0) }

  "delta removes elements, leaving duplicates, and using a custom comparison function".p
    "for example, comparing only the second element of a pair" >> {
      val compare = (p: (Int, Symbol), o: Symbol) => p._2 == o
      Seq((1, 'a), (2, 'b), (3, 'c), (4, 'b), (5, 'e)).delta(Seq('c, 'b, 'a), compare) must_==  Seq((4, 'b), (5, 'e))
    }

  "A removeFirst function" should {
    "remove the first element satisfying a predicate" in {

      "Seq"           | "Element to remove"  | "Result"        |>
      (Nil:Seq[Int])  ! 2                    ! (Nil:Seq[Int])  |
       Seq(2, 3, 4)   ! 2                    ! Seq(3, 4)       |
       Seq(1, 2, 2)   ! 2                    ! Seq(1, 2)       |
       Seq(1, 2, 3)   ! 2                    ! Seq(1, 3)       | { (l, a, r) =>
        l.removeFirst(_ == a) must_== r
      }
    }
    "this should work for any Seq and any element" in Prop.forAll { (l: List[Int], a: Int) =>
      val removed = l removeFirst (_ == a)

      val (withoutA, startWithA) = l span (_ != a)
      removed == withoutA ++ startWithA.drop(1)
    }
  }

  "A difference between 2 seqs can be computed with a specific equality function" >> {
    case class A(i: Int = 0, j: Int = 1)
    val equality: (A, A) => Boolean = (a1: A, a2: A) => a1.i == a2.i

    Seq(A(i = 1), A(i = 2)).difference(Seq(A(i = 2, j = 2)), equality) === Seq(A(i = 1))
  }

}

import org.scalacheck._
import Test._
import execute._

trait ScalaCheckResult {
  implicit def propAsResult: AsResult[Prop] = new AsResult[Prop] {
    def asResult(prop: =>Prop) = {
      Test.check(Parameters.default, prop).status match {
        case `Passed` | Proved(_)           => Success()
        case Failed(args, labels)           => Failure("Property failed with args: "+args.mkString(", ")+" and labels "+labels.mkString(", "))
        case PropException(args, e, labels) => Error(e).updateMessage("Property failed with args: "+args.mkString(", ")+" and labels "+labels.mkString(", "))
        case `Exhausted`                    => Error("exhausted")
      }
    }
  }
}