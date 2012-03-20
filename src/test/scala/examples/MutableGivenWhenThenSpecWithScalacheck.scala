package examples

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck._
import Gen._

class MutableGivenWhenThenSpecWithScalacheck extends Specification with ScalaCheck {

  "A given-when-then example for a calculator".txt.br

//    "Given a first number n1" << {
//      n1 = choose(-10, 10)
//    }
//    "And a second number n2" << {
//      n2 = choose(-10, 10)
//    }
//    "When I add them" << {
//      operation = Arbitrary { for (a1 <- n1; a2 <- n2) yield Addition(a1, a2) }
//    }
//    "Then I should get n1 + n2" << check { (op: Addition) =>
//      op.calculate must_== op.n1 + op.n2
//    }

  var n1, n2: Gen[Int] = choose(0, 0)
  implicit var operation: Arbitrary[Addition] = null

  case class Addition(n1: Int, n2: Int) {
    def calculate: Int = n1 + n2
  }

}
