package examples

import org.specs2._
import org.scalacheck._
import Gen._
import Prop._
import specification.gen._

/**
 * This specification is an acceptance specification written
 */
class GivenWhenThenSpecWithScalacheck extends Specification with ScalaCheck { def is =

  "A given-when-then example for a calculator"                                   ^ br^
    "Given a first number n1"                                                    ^ number1^
    "And a second number n2"                                                     ^ number2^
    "When I add them"                                                            ^ add^
    "Then I should get n1 + n2"                                                  ^ addResult^
    "And it should be positive if the numbers are positive"                      ^ positiveAdd^
                                                                                 endp^
  "Now with the multiplication"                                                  ^ br^
    "Given a first number n1"                                                    ^ number1^
    "And a second number n2"                                                     ^ number2^
    "When I multiply them"                                                       ^ mult^
    "Then I should get n1 * n2"                                                  ^ multResult^
    "And it should be positive if the numbers have the same sign"                ^ positiveMult^
                                                                                 end

  object number1 extends Given[Int] {
    def extract(text: String) = choose(-10, 10)
  }
  object number2 extends When[Int, (Int, Int)] {
    def extract(number1: Int, text: String) = for { n2 <- choose(-10, 10) } yield (number1, n2)
  }
  object add extends When[(Int, Int), Addition] {
    def extract(numbers: (Int, Int), text: String) = Addition(numbers._1, numbers._2)
  }
  object mult extends When[(Int, Int), Multiplication] {
    def extract(numbers: (Int, Int), text: String) = Multiplication(numbers._1, numbers._2)
  }

  object addResult extends Then[Addition] {
    def extract(text: String)(implicit op: Arbitrary[Addition]) = {
      check { (op: Addition) => op.calculate must_== op.n1 + op.n2 }
    }
  }
  object multResult extends Then[Multiplication] {
    def extract(text: String)(implicit op: Arbitrary[Multiplication]) = {
      check { (op: Multiplication) => op.calculate must_== op.n1 * op.n2 }
    }
  }
  object positiveAdd extends Then[Addition] {
    def extract(text: String)(implicit op: Arbitrary[Addition]) = {
      forAll { (a:Addition) =>
        (a.n1 >= 0 && a.n2 >= 0) ==> (a.calculate >= 0)
      }
    }
  }
  object positiveMult extends Then[Multiplication] {
    def extract(text: String)(implicit op: Arbitrary[Multiplication]) = {
      forAll { (a:Multiplication) =>
        (a.n1 >= 0 && a.n2 >= 0) ==> (a.calculate >= 0)
      }
    }
  }

  abstract class Operation {
    def calculate: Int
  }
  case class Multiplication(n1: Int, n2: Int) extends Operation {
    def calculate: Int = n1 * n2
  }
  case class Addition(n1: Int, n2: Int) extends Operation {
    def calculate: Int = n1 + n2
  }

}